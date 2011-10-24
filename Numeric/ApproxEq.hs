-- |
-- Module    : Numeric.ApproxEq
-- Copyright : (c) 2011 Aleksey Khudyakov, Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : Aleksey Khudyakov <alexey.skladnoy@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- Different implementations of approximate equality for floating
-- point values. There are multiple ways to implement approximate
-- equality. They have different semantics and it's up to programmer
-- to choose right one.
module Numeric.ApproxEq (
    eqRelative
  , eqRelCompl
  , eqAbsolute
  , within
  ) where

import Control.Monad.ST         (runST)

import Data.Complex             (Complex(..))
import Data.Primitive.ByteArray (newByteArray, readByteArray, writeByteArray)
import Data.Int                 (Int64)



-- | Relative difference between two numbers are less than predefined
--   value.  For example 1 is approximately equal to 1.0001 with 1e-4
--   precision. Same is true for 10000 and 10001.
--
--   This method of camparison doesn't work for numbers which are
--   approximately 0. 'eqAbsolute' should be used instead.
eqRelative :: (Fractional a, Ord a)
           => a            -- ^ Relative precision
           -> a -> a -> Bool
eqRelative eps a b = abs (a - b) <= abs (eps * a)
{-# INLINE eqRelative #-}

-- | Relative equality for comlex numbers. 
eqRelCompl :: (RealFloat a, Ord a)
           => a                 -- ^ Relative precision
           -> Complex a -> Complex a -> Bool
eqRelCompl eps a b = d <= eps * r
  where
    (d :+ _) = abs (a - b)
    (r :+ _) = abs a
{-# INLINE eqRelCompl #-}

-- | Difference between values is less than specified precision.
eqAbsolute :: (Num a, Ord a)
           => a                 -- ^ Absolute precision
           -> a -> a -> Bool
eqAbsolute eps a b = abs (a - b) <= eps
{-# INLINE eqAbsolute #-}


-- | Compare two 'Double' values for approximate equality, using
-- Dawson's method.
--
-- The required accuracy is specified in ULPs (units of least
-- precision).  If the two numbers differ by the given number of ULPs
-- or less, this function returns @True@.
--
-- Algorithm is based on Bruce Dawson's \"Comparing floating point
-- numbers\":
-- <http://www.cygnus-software.com/papers/comparingfloats/comparingfloats.htm>
within :: Int                   -- ^ Number of ULPs of accuracy desired.
       -> Double -> Double -> Bool
within ulps a b = runST $ do
  buf <- newByteArray 8
  ai0 <- writeByteArray buf 0 a >> readByteArray buf 0
  bi0 <- writeByteArray buf 0 b >> readByteArray buf 0
  let big  = 0x8000000000000000 :: Int64
      ai | ai0 < 0   = big - ai0
         | otherwise = ai0
      bi | bi0 < 0   = big - bi0
         | otherwise = bi0
  return $! abs (ai - bi) <= fromIntegral ulps

