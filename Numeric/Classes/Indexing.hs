{-# LANGUAGE TypeFamilies #-}
-- |
-- Module    : Numeric.Classes.Indexing
-- Copyright : (c) 2011 Aleksey Khudyakov
-- License   : BSD3
--
-- Maintainer  : Aleksey Khudyakov <alexey.skladnoy@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
module Numeric.Classes.Indexing (
    Indexable(..)
  , validIndex
  ) where

import qualified Data.Vector          as V 
import qualified Data.Vector.Unboxed  as U
import qualified Data.Vector.Storable as S



-- | Type class for array-like data type which support @O(1)@ access
--   by integer index starting from zero.
class Indexable a where
  type IndexVal a :: *
  -- | Size of table.
  size        :: a -> Int
  -- | /O(1)/ Index table without range cheking.
  unsafeIndex :: a -> Int -> IndexVal a
  -- | /O(1)/ Safe indexing. Calls error if index is out of range.
  (!)         :: a -> Int -> IndexVal a
  x ! i | i < 0 || i > size x = error "Numeric.Classes.Indexing.!: index is out of range"
        | otherwise           = unsafeIndex x i
  {-# INLINE (!) #-}

-- | Check that index is valid
validIndex :: Indexable a => a -> Int -> Bool 
validIndex tbl i = i >= 0 && i < size tbl
{-# INLINE validIndex #-}

instance Indexable (V.Vector a) where
  type IndexVal (V.Vector a) = a
  size        = V.length
  unsafeIndex = V.unsafeIndex
  (!)         = (V.!)
  {-# INLINE size        #-}
  {-# INLINE (!)         #-}
  {-# INLINE unsafeIndex #-}

instance U.Unbox a => Indexable (U.Vector a) where
  type IndexVal (U.Vector a) = a
  size        = U.length
  unsafeIndex = U.unsafeIndex
  (!)         = (U.!)
  {-# INLINE size        #-}
  {-# INLINE (!)         #-}
  {-# INLINE unsafeIndex #-}

instance S.Storable a => Indexable (S.Vector a) where
  type IndexVal (S.Vector a) = a
  size        = S.length
  unsafeIndex = S.unsafeIndex
  (!)         = (S.!)
  {-# INLINE size        #-}
  {-# INLINE (!)         #-}
  {-# INLINE unsafeIndex #-}
