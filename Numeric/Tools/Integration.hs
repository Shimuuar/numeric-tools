{-# LANGUAGE BangPatterns #-}
module Numeric.Tool.Integration (
  ) where

import Numeric.Utils

nextTrapezoid :: Double             -- Lower integration limit
              -> Double             -- Upper integration limit
              -> Int                -- Number of additional points
              -> (Double -> Double) -- Function to integrate
              -> Double             -- Correction
nextTrapezoid from to n f = sep * (sum' $ map f $ generate n (+sep) x0)
  where
    sep = (to - from) / fromIntegral n -- Separation between points
    x0  = from + 0.5 * sep


-- | Trapezoidal integration
quadTrapeziod :: (Double, Double)   -- ^ Integration limit
              -> Double             -- ^ Precision
              -> (Double -> Double) -- ^ Function to integrate
              -> Double
quadTrapeziod (from,to) eps f = worker 1 (0.5 * (to - from) * (f to + f from))
  where
    worker n appr 
      | n > 2^5 && abs (appr' - appr) < eps * abs appr = appr'
      | otherwise                                      = worker (n*2) appr'
      where
        appr' = 0.5 * (appr + nextTrapezoid from to n f)
