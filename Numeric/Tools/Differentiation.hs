module Numeric.Tool.Differentiation (
    diffSimple
  , diffSimmetric
  -- , diffRichardson
  , representableDelta 
  ) where

import Control.Monad.ST (runST)
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as M

import Numeric.FloatingPoint



-- | Simplest form of differentiation. Should be used only when
--   function evaluation is prohibitively expensive and value at point
--   @x@ should be reused.
--
--   > f'(x) = f(x+h) - f(x) / h
diffSimple :: (Double -> Double) -- ^ Function to differentiate
           -> Double             -- ^ Delta
           -> (Double,Double)    -- ^ Coordinate and function value at this point
           -> Double
diffSimple f h (x,fx) = (f (x + h') - fx) / h' where h' = representableDelta x h
{-# INLINE diffSimple #-}                                                     


-- | Simple differentiation. It uses simple rule and provide
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
  let nMax = 10
  let con  = 1.4
      con2 = con*con
  let safe = 2 
  arr <- M.new nMax
  -- Initial approximation
  let d0 = (f (x + h) - f (x - h)) / (2*h)
  M.write arr 0 d0
  -- -- 
  let worker i hh err ans 
        | i == nMax = return ans
        | otherwise = do
            let richard j fac x err' ans'
                  | j > i     = do
                      M.write arr (j-1) x
                      return (ans',err')
                  | otherwise = do
                      xOld <- M.read arr (j-1)
                      M.write arr (j-1) x
                      let x'   = (x*fac - xOld) / (fac - 1)
                          errt = max (abs $ x' - x) (abs $ x' - xOld)
                      if errt < err' 
                        then richard (j+1) (fac * con2) x' errt x'
                        else richard (j+1) (fac * con2) x' err' ans
            -- 
            let hh' = hh / con
                d   = (f (x + hh') - f (x - hh')) / (2 * hh')
            (ans',err') <- richard 1 con2 d err ans
            x'  <- M.read arr i
            x'' <- M.read arr i
            if( abs (x' - x'') >= safe * err' )
              then return ans'
              else worker (i+1) hh' err' ans'
  -- 
  worker 1 h posInfty nan



      
----------------------------------------------------------------


-- | For number @x@ and small @h@ return such @h'@ that @x+h'@ and @x@
-- differ by representable number
representableDelta :: Double    -- ^ x
                   -> Double    -- ^ small delta
                   -> Double 
representableDelta x h = let temp = x + h in temp - x
-- FIXME: It's wrong. I'm sure
