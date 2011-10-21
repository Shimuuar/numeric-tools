{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
    -- * Data type
    Root(..)
  , fromRoot
    -- * Equations solversv
  , solveBisection
  , solveRidders  
  -- * References
  -- $references
  ) where

import Numeric.ApproxEq

import Control.Applicative
import Control.Monad       (MonadPlus(..), ap)
import Data.Typeable       (Typeable)



-- | The result of searching for a root of a mathematical function.
data Root a = NotBracketed
            -- ^ The function does not have opposite signs when
            -- evaluated at the lower and upper bounds of the search.
            | SearchFailed
            -- ^ The search failed to converge to within the given
            -- error tolerance after the given number of iterations.
            | Root a
            -- ^ A root was successfully found.
              deriving (Eq, Read, Show, Typeable)

instance Functor Root where
  fmap _ NotBracketed = NotBracketed
  fmap _ SearchFailed = SearchFailed
  fmap f (Root a)     = Root (f a)

instance Monad Root where
  NotBracketed >>= _ = NotBracketed
  SearchFailed >>= _ = SearchFailed
  Root a       >>= m = m a
  return = Root

instance MonadPlus Root where
  mzero = SearchFailed
  r@(Root _) `mplus` _ = r
  _          `mplus` p = p

instance Applicative Root where
  pure  = Root
  (<*>) = ap

instance Alternative Root where
  empty = SearchFailed
  r@(Root _) <|> _ = r
  _          <|> p = p

-- | Returns either the result of a search for a root, or the default
-- value if the search failed.
fromRoot :: a                   -- ^ Default value.
         -> Root a              -- ^ Result of search for a root.
         -> a
fromRoot _ (Root a) = a
fromRoot a _        = a


-- | Solve equation @f(x) = 0@ using bisection method. Function is
--   must be continous. If function has different signs at the ends of
--   initial interval answer is always returned. 'Nothing' is returned
--   if function fails to find an answer.
solveBisection :: Double             -- ^ Required absolute precision
               -> (Double,Double)    -- ^ Range
               -> (Double -> Double) -- ^ Equation
               -> Root Double
solveBisection eps (lo,hi) f
  | flo * fhi > 0 = NotBracketed
  | flo == 0      = Root lo
  | fhi == 0      = Root hi
  | otherwise     = worker 0 lo flo hi fhi
  where
    flo = f lo
    fhi = f hi
    worker i a fa b fb
      | within 1 a b      = Root a
      | (b - a) <= eps    = Root c
      | fc == 0           = Root c
      | i >= (100 :: Int) = SearchFailed
      | fa * fc < 0       = worker (i+1) a fa c fc
      | otherwise         = worker (i+1) c fc b fb
      where
        c  = 0.5 * (a + b)
        fc = f c


-- | Use the method of Ridders to compute a root of a function.
--
-- The function must have opposite signs when evaluated at the lower
-- and upper bounds of the search (i.e. the root must be bracketed).
solveRidders :: Double               -- ^ Absolute error tolerance.
             -> (Double,Double)      -- ^ Lower and upper bounds for the search.
             -> (Double -> Double)   -- ^ Function to find the roots of.
             -> Root Double
solveRidders eps (lo,hi) f
    | flo == 0    = Root lo
    | fhi == 0    = Root hi
    | flo*fhi > 0 = NotBracketed -- root is not bracketed
    | otherwise   = go lo flo hi fhi 0
  where
    go !a !fa !b !fb !i
        -- Root is bracketed within 1 ulp. No improvement could be made
        | within 1 a b       = Root a
        -- Root is found. Check that f(m) == 0 is nessesary to ensure
        -- that root is never passed to 'go'
        | fm == 0            = Root m
        | fn == 0            = Root n
        | d < eps            = Root n
        -- Too many iterations performed. Fail
        | i >= (100 :: Int)  = SearchFailed
        -- Ridder's approximation coincide with one of old
        -- bounds. Revert to bisection
        | n == a || n == b   = case () of
          _| fm*fa < 0 -> go a fa m fm (i+1)
           | otherwise -> go m fm b fb (i+1)
        -- Proceed as usual
        | fn*fm < 0          = go n fn m fm (i+1)
        | fn*fa < 0          = go a fa n fn (i+1)
        | otherwise          = go n fn b fb (i+1)
      where
        d    = abs (b - a)
        dm   = (b - a) * 0.5
        !m   = a + dm
        !fm  = f m
        !dn  = signum (fb - fa) * dm * fm / sqrt(fm*fm - fa*fb)
        !n   = m - signum dn * min (abs dn) (abs dm - 0.5 * eps)
        !fn  = f n
    !flo = f lo
    !fhi = f hi


-- $references
--
-- * Ridders, C.F.J. (1979) A new algorithm for computing a single
--   root of a real continuous function.
--   /IEEE Transactions on Circuits and Systems/ 26:979&#8211;980.

