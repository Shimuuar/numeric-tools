{-# LANGUAGE BangPatterns #-}
module Numeric.Tool.Integration (
  ) where

import Debug.Trace
import Numeric.Utils

----------------------------------------------------------------
-- Trapezoid and Simpson integraion
----------------------------------------------------------------

-- Initial guess for trapezoid rule
trapGuess :: Double -> Double -> (Double -> Double) -> Double
trapGuess a b f = 0.5 * (b - a) * (f b + f a)


-- Refinement of guess using trapeziod algorithms
nextTrapezoid :: Double             -- Lower integration limit
              -> Double             -- Upper integration limit
              -> Int                -- Number of additional points
              -> (Double -> Double) -- Function to integrate
              -> Double             -- Approximation
              -> Double
nextTrapezoid a b n f q = 0.5 * (q + sep * s)
  where
    sep = (b - a) / fromIntegral n            -- Separation between points
    x0  = a + 0.5 * sep                       -- Starting point
    s   = sum' $ map f $ generate n (+sep) x0 -- Sum of all points

-- | Trapezoidal integration
quadTrapeziod :: (Double, Double)   -- ^ Integration limit
              -> Double             -- ^ Precision
              -> (Double -> Double) -- ^ Function to integrate
              -> Double
quadTrapeziod (a,b) eps f = worker 1 (trapGuess a b f)
  where
    worker n q
      | n > 2^5 && abs (q' - q) < eps * abs q = q'
      | otherwise                             = worker (n*2) q'
      where
        q' = nextTrapezoid a b n f q
-- FIXME: failure to converge?

quadSimpson :: (Double, Double)   -- ^ Integration limit
            -> Double             -- ^ Precision
            -> (Double -> Double) -- ^ Function to integrate
            -> Double
quadSimpson (a,b) eps f = worker 1 0 (trapGuess a b f)
  where
    worker n s st 
      | n > 2^5 && abs (s' - s) < eps * abs s = s'
      | otherwise                             = worker (n*2) s' st'
      where
        st' = nextTrapezoid a b n f st
        s'  = (4*st' - st) / 3
-- FIXME: failure to converge?
