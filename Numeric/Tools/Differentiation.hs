{-# LANGUAGE ForeignFunctionInterface #-}
module Numeric.Tools.Differentiation (
    diffSimple
  , diffSimmetric
  , diffRichardson
    -- * Utils
  , representableDelta 
  ) where

import Control.Monad.ST (runST)
import Control.Monad.Primitive
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as M
import Foreign
import Foreign.C

import Numeric.FloatingPoint

import Debug.Trace
import Text.Printf

-- | Simplest form of differentiation. Should be used only when
--   function evaluation is prohibitively expensive and already
--   computed value at point @x@ should be reused.
--
--   > f'(x) = f(x+h) - f(x) / h
diffSimple :: (Double -> Double) -- ^ Function to differentiate
           -> Double             -- ^ Delta
           -> (Double,Double)    -- ^ Coordinate and function value at this point
           -> Double
diffSimple f h (x,fx) = (f (x + h') - fx) / h' where h' = representableDelta x h
{-# INLINE diffSimple #-}                                                     


-- | Simple differentiation. It uses simmetric rule and provide
--   reasonable accuracy. It's suitable when function evaluation is
--   expensive and precision could be traded for speed.
--
-- > f'(x) = f(x-h) + f(x+h) / 2h
diffSimmetric :: (Double -> Double) -- ^ Function to differentiate
              -> Double             -- ^ Delta
              -> Double             -- ^ Point at which evaluate differential
              -> Double
diffSimmetric f h x = (f(x + h) - f(x - h)) / (2 * h)
  where
    h = representableDelta x h


diffRichardson :: (Double -> Double) -- ^ Function
               -> Double             -- ^ Delta
               -> Double             -- ^ Point at which evaluate differential
               -> Double
diffRichardson f h x = runST $ do
  let nMax = 10                 -- Maximum number of iterations
  let con  = 1.4                -- Decrement for step size
      con2 = con*con            -- Square of decrement
  let safe = 2
  -- Start calculations
  arr <- M.new nMax
  let worker i hh err ans = do
        -- Calculate extrapolations
        let richard j fac x err' ans' = do
              xOld <- replace arr (j-1) x
              case () of
                _| j > i     -> return (ans',err')
                 | otherwise -> 
                   let x'   = (x*fac - xOld) / (fac - 1)           -- New extrapolation
                       errt = max (abs $ x' - x) (abs $ x' - xOld) -- New error estimate
                       (ans'',err'') = if errt < err' then (x'   , errt)
                                                      else (ans' , err')
                   in richard (j+1) (fac*con2) x' err'' ans''
        -- Main loop
        let hh' = hh / con                                -- New step size
            d   = (f (x + hh') - f (x - hh')) / (2 * hh') -- New approximation
        x'  <- M.read arr (i-1)
        (ans',err') <- richard 1 con2 d err ans
        x'' <- M.read arr i
        case () of
          _| abs (x' - x'') >= safe * err' -> return ans'
           | i >= nMax - 1                 -> return ans'
           | otherwise                     -> worker (i+1) hh' err' ans'
  -- Calculate
  M.write arr 0 $ (f (x + h) - f (x - h)) / (2*h)
  worker 1 h posInfty nan



      
----------------------------------------------------------------

-- replace :: (PrimMonad m, M.MVector v a) => v (PrimState m) a -> Int -> a -> m a
replace arr i x = do
  x' <- M.read arr i
  M.write arr i x
  return x'
{-# INLINE replace #-}
  

-- | For number @x@ and small @h@ return such @h'@ that @x+h'@ and @x@
-- differ by representable number
representableDelta :: Double    -- ^ x
                   -> Double    -- ^ small delta
                   -> Double 
representableDelta x h = realToFrac $ unsafePerformIO $ representableDeltaFFI (realToFrac x) (realToFrac h)
{-# INLINE representableDelta #-}

foreign import ccall "numeric_tools_representable_delta" 
  representableDeltaFFI :: CDouble -> CDouble -> IO CDouble