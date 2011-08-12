{-# LANGUAGE TypeFamilies #-}
module Numeric.Classes.Indexing (
    Indexable(..)
  ) where

import qualified Data.Vector          as V 
import qualified Data.Vector.Unboxed  as U
import qualified Data.Vector.Storable as S



-- | 
class Indexable a where
  type IndexVal a :: *
  -- | Size of table
  size        :: a -> Int
  -- | Index table without range cheking
  unsafeIndex :: a -> Int -> IndexVal a
  -- | Safe indexing.
  (!)         :: a -> Int -> IndexVal a
  x ! i | i < 0 || i > size x = error "Numeric.Classes.Indexing.!: index is out of range"
        | otherwise           = unsafeIndex x i

instance Indexable (V.Vector a) where
  type IndexVal (V.Vector a) = a
  size        = V.length
  unsafeIndex = V.unsafeIndex
  (!)         = (V.!)

instance U.Unbox a => Indexable (U.Vector a) where
  type IndexVal (U.Vector a) = a
  size        = U.length
  unsafeIndex = U.unsafeIndex
  (!)         = (U.!)

instance S.Storable a => Indexable (S.Vector a) where
  type IndexVal (S.Vector a) = a
  size        = S.length
  unsafeIndex = S.unsafeIndex
  (!)         = (S.!)
