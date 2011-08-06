module Numeric.Utils (
    sum'
  , nap
  , generate
  ) where

import Data.List

-- | Strict sum
sum' :: Num a => [a] -> a
sum' = foldl' (+) 0
{-# INLINE sum' #-}

-- | Apply function n times
nap :: Int -> (a -> a) -> a -> a
nap 0 _ x = x
nap n f x = nap (n-1) f $! f  x
{-# INLINE nap #-}

-- | Generate 
generate :: Int -> (a -> a) -> a -> [a]
generate 0 _ _ = []
generate n f x = x : generate (n-1) f (f x)
{-# INLINE generate #-}