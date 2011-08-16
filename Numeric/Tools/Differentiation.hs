module Numeric.Tool.Differentiation (
    diffSimple
  , diffSimmetric
  -- , diffRichardson
  , representableDelta 
  ) where

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


-- diffRichardson :: (Double -> Double) -- ^ Function
--               -> Double             -- ^ Delta
--               -> Double             -- ^ Point at which evaluate differential
--               -> Double
-- diffRichardson f h x = 0                  

----------------------------------------------------------------


-- | For number @x@ and small @h@ return such @h'@ that @x+h'@ and @x@
-- differ by representable number
representableDelta :: Double    -- ^ x
                   -> Double    -- ^ small delta
                   -> Double 
representableDelta x h = let temp = x + h in temp - x
-- FIXME: It's wrong. I'm sure
