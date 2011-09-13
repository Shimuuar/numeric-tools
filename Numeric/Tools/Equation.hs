-- |
-- Module    : Numeric.Tools.Equation
-- Copyright : (c) 2011 Aleksey Khudyakov
-- License   : BSD3
--
-- Maintainer  : Aleksey Khudyakov <alexey.skladnoy@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- Numerical solution of ordinary equations.
module Numeric.Tools.Equation ( 
    solveBisection
  ) where

import Numeric.IEEE (epsilon)



-- | Solve equation @f(x) = 0@ using bisection method. Function is
--   must be continous. If function has different signs at the ends of
--   initial interval answer is always returned. 'Nothing' is returned
--   if function fails to find an answer.
solveBisection :: Double             -- ^ Required absolute precision
               -> (Double,Double)    -- ^ Range
               -> (Double -> Double) -- ^ Equation
               -> Maybe Double
solveBisection eps (a,b) f
  | a >= b      = Nothing
  | fa * fb > 0 = Nothing
  | otherwise   = Just $ bisectionWorker (abs eps) f a b fa fb
  where
    fa = f a
    fb = f b

bisectionWorker :: Double -> (Double -> Double) -> Double -> Double -> Double -> Double -> Double
bisectionWorker eps f a b fa fb
  | (b - a)     <= eps     = c
  | (b - a) / b <= epsilon = c
  | fa * fc < 0            = bisectionWorker eps f a c fa fc
  | otherwise              = bisectionWorker eps f c b fc fb
  where
    c  = 0.5 * (a + b)
    fc = f c

