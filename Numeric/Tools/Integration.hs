{-# LANGUAGE BangPatterns #-}
-- |
-- Module    : Numeric.Tools.Integration
-- Copyright : (c) 2011 Aleksey Khudyakov
-- License   : BSD3
--
-- Maintainer  : Aleksey Khudyakov <alexey.skladnoy@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- Funtions for numerical instegration.
--
module Numeric.Tools.Integration (
    -- * Integration parameters
    QuadParam(..)
  , defQuad
  , QuadRes(..)
    -- * Integration functions
  , quadTrapezoid
  , quadSimpson
  , quadRomberg
  ) where

import Control.Monad.ST

import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as M

import Debug.Trace



----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

-- | Integration parameters for numerical routines. Note that each
-- additional iteration doubles number of function evaluation required
-- to compute integral. 
--
-- Number of iterations is capped at 30.
data QuadParam = QuadParam { 
    quadPrecision :: Double -- ^ Relative precision of answer
  , quadMaxIter   :: Int    -- ^ Maximum number of iterations
  }
  deriving (Show,Eq)

-- Number of iterations limited to 30
maxIter :: QuadParam -> Int
maxIter = min 30 . quadMaxIter

-- | Default parameters for integration functions
--
-- * Maximum number of iterations = 20
-- * Precision is 1e-9
defQuad :: QuadParam
defQuad =  QuadParam { quadPrecision = 1e-9
                     , quadMaxIter   = 20
                     }

-- | Result of numeric integration
data QuadRes = QuadRes { quadRes     :: Maybe Double -- ^ Integraion result
                       , quadPrecEst :: Double       -- ^ Rough estimate of attained precision
                       , quadNIter   :: Int          -- ^ Number of iterations
                       }
               deriving (Show,Eq)



----------------------------------------------------------------
-- Different integration methods
----------------------------------------------------------------

-- | Integration of using trapezoids.
quadTrapezoid :: QuadParam          -- ^ Precision
              -> (Double, Double)   -- ^ Integration limit
              -> (Double -> Double) -- ^ Function to integrate
              -> QuadRes
quadTrapezoid param (a,b) f = worker 1 1 (trapGuess a b f)
  where
    eps  = quadPrecision param  -- Requred precision
    maxN = maxIter param        -- Maximum allowed number of iterations
    worker n nPoints q
      | n > 5 && d < eps = ret (Just q')
      | n >= maxN        = ret Nothing 
      | otherwise        = worker (n+1) (nPoints*2) q'
      where
        q'  = nextTrapezoid a b nPoints f q -- New approximation
        d   = abs (q' - q) / abs q          -- Precision estimate
        ret = \x -> QuadRes x d n

-- | Integration using Simpson rule.
quadSimpson :: QuadParam          -- ^ Precision
            -> (Double, Double)   -- ^ Integration limit
            -> (Double -> Double) -- ^ Function to integrate
            -> QuadRes
quadSimpson param (a,b) f = worker 1 1  0 (trapGuess a b f)
  where
    eps  = quadPrecision param  -- Requred precision
    maxN = maxIter param        -- Maximum allowed number of points for evaluation
    worker n nPoints s st 
      | n > 5 && d < eps = ret (Just s')
      | n >= maxN        = ret Nothing
      | otherwise        = worker (n+1) (nPoints*2) s' st'
      where
        st' = nextTrapezoid a b nPoints f st
        s'  = (4*st' - st) / 3
        d   = abs (s' - s) / abs s
        ret = \x -> QuadRes x d n

-- | Integration using Romberg rule.
quadRomberg :: QuadParam          -- ^ Precision
            -> (Double, Double)   -- ^ Integration limit
            -> (Double -> Double) -- ^ Function to integrate
            -> QuadRes
quadRomberg param (a,b) f =
  runST $ do 
    let eps  = quadPrecision param
        maxN = maxIter       param
    arr <- M.new maxN
    -- Calculate new approximation
    let nextAppr n s = runNextAppr 0 4 s where
          runNextAppr i fac s = do
            x <- M.read arr i
            M.write arr i s
            if i >= n
              then return s
              else runNextAppr (i+1) (fac*4) $ s + (s - x) / (fac - 1)
    -- Maine loop
    let worker n nPoints st s = do
          let st' = nextTrapezoid a b nPoints f st
          s' <- M.write arr 0 st >> nextAppr n st'
          let d = abs (s' - s) / abs s
          case () of 
            _ | n > 5 && d < eps -> return $ QuadRes (Just s') d n
              | n >= maxN        -> return $ QuadRes Nothing   d n
              | otherwise        -> worker (n+1) (nPoints*2) st' s'
    -- Calculate integral
    worker 1 1 st0 st0 where  st0 = trapGuess a b f



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
    sep = (b - a) / fromIntegral n                  -- Separation between points
    x0  = a + 0.5 * sep                             -- Starting point
    s   = U.sum $ U.map f $ U.iterateN n (+sep) x0  -- Sum of all points
