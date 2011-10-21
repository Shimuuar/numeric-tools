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
  ) where

import Numeric.IEEE        (epsilon)

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
solveBisection eps (a,b) f
  | fa * fb > 0 = NotBracketed
  | otherwise   = Root $ bisectionWorker (abs eps) f a b fa fb
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

