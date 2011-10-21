-- |
-- Module    : Numeric.ApproxEq
-- Copyright : (c) 2011 Aleksey Khudyakov, Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : Aleksey Khudyakov <alexey.skladnoy@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- Different implementations of approximate equalities of floating
-- point values.
module Numeric.ApproxEq (
    within
  ) where

import Control.Monad.ST         (runST)
import Data.Primitive.ByteArray (newByteArray, readByteArray, writeByteArray)
import Data.Int                 (Int64)

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
