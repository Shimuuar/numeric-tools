{-# LANGUAGE BangPatterns #-}
module Numeric.Tool.Integration (
    -- * Integration parameters
    QuadParam(..)
  , defQuad
    -- * Integration functions
  , quadTrapeziod
  , quadSimpson
  ) where

import Debug.Trace
import Numeric.Utils

----------------------------------------------------------------
-- Trapezoid and Simpson integration
----------------------------------------------------------------

-- | Integration parameters for numerical routines
data QuadParam = QuadParam { 
    quadPrecision :: Double -- ^ Relative precision of answer
  , quadMaxIter   :: Int    -- ^ Maximum number of iterations
  }
  deriving (Show,Eq)

-- | Default parameters for integration functions
defQuad :: QuadParam
defQuad =  QuadParam { quadPrecision = 1e-9
                     , quadMaxIter   = 20
                     }

-- | Trapezoidal integration. Returns 'Nothing' if integral fails to
--   converge
quadTrapeziod :: (Double, Double)   -- ^ Integration limit
              -> QuadParam          -- ^ Precision
              -> (Double -> Double) -- ^ Function to integrate
              -> Maybe Double
quadTrapeziod (a,b) param f = worker 1 (trapGuess a b f)
  where
    eps = quadPrecision param
    worker n q
      | n > quadMaxIter param                 = Nothing 
      | n > 2^5 && abs (q' - q) < eps * abs q = Just q'
      | otherwise                             = worker (n*2) q'
      where
        q' = nextTrapezoid a b n f q

-- | Simpson rule
quadSimpson :: (Double, Double)   -- ^ Integration limit
            -> QuadParam          -- ^ Precision
            -> (Double -> Double) -- ^ Function to integrate
            -> Maybe Double
quadSimpson (a,b) param f = worker 1 0 (trapGuess a b f)
  where
    eps = quadPrecision param
    worker n s st 
      | n > quadMaxIter param                 = Nothing
      | n > 2^5 && abs (s' - s) < eps * abs s = Just s'
      | otherwise                             = worker (n*2) s' st'
      where
        st' = nextTrapezoid a b n f st
        s'  = (4*st' - st) / 3

-- FIXME: Romberg integration

----------------------------------------------------------------
-- Helpers
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
