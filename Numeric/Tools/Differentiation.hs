module Numeric.Tool.Differentiation (
  ) where

import Numeric.FloatingPoint

-- | Simple differentiation. It uses simple rule and provide
--   reasonable accuracy. It's suitable when function evaluation is
--   expensive and precision could be traded for speed.
--
-- > f'(x) = f(x-h) + f(x+h) / 2h
simpleDiff :: (Double -> Double) -- ^ Function 
           -> Double             -- ^ Point at which evaluate differential
           -> Double
simpleDiff f x = (f(x + h) - f(x - h)) / (2 * h)
  where
    h = representableDelta x (sqrt epsilon * x)
    

-- | For number @x@ and small @h@ return such @h'@ that @x+h'@ and @x@
-- differ by representable number
representableDelta :: Double    -- ^ x
                   -> Double    -- ^ small delta
                   -> Double 
representableDelta x h = let temp = x + h in temp - x
-- FIXME: It's wrong. I'm sure
