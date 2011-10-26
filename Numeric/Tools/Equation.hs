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
  , solveNewton
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


-- | Use bisection method to compute root of function.
--
-- The function must have opposite signs when evaluated at the lower
-- and upper bounds of the search (i.e. the root must be bracketed).
solveBisection :: Double             -- ^ Required absolute precision
               -> (Double,Double)    -- ^ Range
               -> (Double -> Double) -- ^ Equation
               -> Root Double
solveBisection eps (lo,hi) f
  | flo * fhi > 0 = NotBracketed
  | flo == 0      = Root lo
  | fhi == 0      = Root hi
  | flo < 0       = worker 0 lo hi
  | otherwise     = worker 0 hi lo
  where
    flo = f lo
    fhi = f hi
    -- Worker function. Preconditions:
    --   f a < 0
    --   f b > 0
    worker i a b
      | within 1 a b       = Root a
      | abs (b - a) <= eps = Root c
      | fc == 0            = Root c
      | i >= (100 :: Int)  = SearchFailed
      | fc < 0             = worker (i+1) c b
      | otherwise          = worker (i+1) a c
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


-- | Solve equation using Newton-Raphson method. Root must be
--   bracketed. If Newton's step jumps outside of bracket or do not
--   converge sufficiently fast function reverts to bisection.
solveNewton :: Double             -- ^ Absolute error tolerance
            -> (Double,Double)    -- ^ Lower and upper bounds for root
            -> (Double -> Double) -- ^ Function
            -> (Double -> Double) -- ^ Function's derivative
            -> Root Double
solveNewton eps (lo,hi) f f'
  | flo == 0      = Root lo
  | fhi == 0      = Root hi
  | flo * fhi > 0 = NotBracketed
  | otherwise     = let d   = 0.5*(hi-lo)
                        mid = 0.5*(lo+hi)
                        fun = worker 0 d mid (f mid) (f' mid)
                    in if flo < 0 then fun lo hi
                                  else fun hi lo     
  where
    flo = f lo
    fhi = f hi
    -- Worker function
    worker i dxOld x fx fx' a b
      -- Convergence achieved
      | fx == 0           = Root x  -- x is a root
      | within 1 a b      = Root x  -- Bracket is too tight. No improvements could be made
      | abs (a - b) < eps = Root x  --
      | abs dx      < eps = Root x' -- Precision achieved
      | within 0 x x'     = Root x' -- Newton step doesn't improve solution
      -- Too many iterations
      | i > (100::Int)    = SearchFailed
      -- Newton step jumped out of range or step decreases too slow.
      -- Revert to bisection.
      -- NOTE this guard is selected if dx evaluated to NaN or ±∞.
      | not ((a - x')*(b - x') < 0) || abs (dxOld / dx) < 2 =
          let dx' = 0.5 * (b - a)
              mid = 0.5 * (a + b)
          in if fx < 0 then step dx' mid
                       else step dx' mid
      -- Normal newton step
      | otherwise =
          if fx < 0 then step dx x' 
                    else step dx x'
      where
        dx = fx / fx'
        x' = x - dx
        -- Perform one step
        step dy y
          | fy < 0    = worker (i+1) dy  y fy fy'  y b
          | otherwise = worker (i+1) dy  y fy fy'  a y
          where fy  = f  y
                fy' = f' y
{-# INLINABLE solveNewton #-}


-- $references
--
-- * Ridders, C.F.J. (1979) A new algorithm for computing a single
--   root of a real continuous function.
--   /IEEE Transactions on Circuits and Systems/ 26:979&#8211;980.

