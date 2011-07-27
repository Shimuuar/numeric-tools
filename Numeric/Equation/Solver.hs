module Numeric.Equation.Solver ( 
    solveBisection
  ) where

solveBisection :: Double             -- ^ Required precision
               -> (Double,Double)    -- ^ Range
               -> (Double -> Double) -- ^ Equation
               -> Maybe Double
solveBisection eps (a,b) f
  | a >= b      = Nothing
  | fa * fb > 0 = Nothing
  | otherwise   = Just $ bisectionWorker eps f a b fa fb
  where
    fa = f a
    fb = f b

bisectionWorker :: Double -> (Double -> Double) -> Double -> Double -> Double -> Double -> Double
bisectionWorker eps f a b fa fb
  | (b - a) < eps = c
  | fa * fc < 0   = bisectionWorker eps f a c fa fc
  | otherwise     = bisectionWorker eps f c b fc fb
  where
    c  = 0.5 * (a + b)
    fc = f c
