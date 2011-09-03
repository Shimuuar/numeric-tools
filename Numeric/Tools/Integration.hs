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
-- Trapezoid and Simpson integration
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
maxIter = max 30 . quadMaxIter

-- | Default parameters for integration functions
--
-- * Maximum number of iterations = 20
-- * Precision is 1e-9
defQuad :: QuadParam
defQuad =  QuadParam { quadPrecision = 1e-9
                     , quadMaxIter   = 20
                     }

----------------------------------------------------------------

-- | Trapezoidal integration. Returns 'Nothing' if integral fails to
--   converge
quadTrapezoid :: QuadParam          -- ^ Precision
              -> (Double, Double)   -- ^ Integration limit
              -> (Double -> Double) -- ^ Function to integrate
              -> Maybe Double
quadTrapezoid param (a,b) f = worker 1 (trapGuess a b f)
  where
    eps  = quadPrecision param  -- Requred precision
    maxN = 2 ^ maxIter param    -- Maximum allowed number of points for evaluation
    worker n q
      | n > maxN                              = Nothing 
      | n > 2^5 && abs (q' - q) < eps * abs q = Just q'
      | otherwise                             = worker (n*2) q'
      where
        q' = nextTrapezoid a b n f q

-- | Simpson rule
quadSimpson :: QuadParam          -- ^ Precision
            -> (Double, Double)   -- ^ Integration limit
            -> (Double -> Double) -- ^ Function to integrate
            -> Maybe Double
quadSimpson param (a,b) f = worker 1 0 (trapGuess a b f)
  where
    eps  = quadPrecision param  -- Requred precision
    maxN = 2 ^ maxIter param    -- Maximum allowed number of points for evaluation
    worker n s st 
      | n > maxN                              = Nothing
      | n > 2^5 && abs (s' - s) < eps * abs s = Just s'
      | otherwise                             = worker (n*2) s' st'
      where
        st' = nextTrapezoid a b n f st
        s'  = (4*st' - st) / 3

-- | Integration using Romberg rule
quadRomberg :: QuadParam          -- ^ Precision
            -> (Double, Double)   -- ^ Integration limit
            -> (Double -> Double) -- ^ Function to integrate
            -> Maybe Double
quadRomberg (QuadParam eps nMax) (a,b) f =
  runST $ do 
    arr <- M.new nMax
    -- Calculate new approximation
    let nextAppr n i s
          | i >= n    = M.write arr n s >> return s
          | otherwise = do
              x <- M.read arr i
              let s' = s + (s - x) / fromIntegral (4^(i+1) - 1)
              id 
               $ trace ("  " ++ show s ++ "  |  " ++ show x)
               $ return ()
              M.write arr i s
              nextAppr n (i+1) s'
    -- Maine loop
    let worker i st s
          | i >= nMax = return Nothing
          | otherwise = do
              let st' = nextTrapezoid a b (2^(i-1)) f st
              M.write arr 0 st
              s' <- nextAppr i 0 st'
              id 
                $ traceShow s'
                $ return ()
              if i > 5 && abs (s' - s) < eps * abs s
                then return $ Just s'
                else worker (i+1) st' s'
    -- Calculate integral
    let st0 = trapGuess a b f
    worker 1 st0 st0



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


blamg :: Double -> Double
blamg x = x^4 * log(x + sqrt (x*x + 1))
