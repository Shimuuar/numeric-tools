{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies       #-}
module Numeric.Tools.Interpolation (
    -- * Type class
    Interpolation(..)
    -- * Data types
  , LinearInterp
  , linearInterpMesh
  , linearInterpTable
  ) where

import Data.Data (Data,Typeable)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic as G

import Numeric.Classes.Indexing
import Numeric.Tools.Mesh

----------------------------------------------------------------

-- | Interpolation for arbitraty meshes
class Interpolation a where
  -- | Interpolate function at some point. Interpolation should not
  --   fail outside of mesh however it may and will give nonsensical
  --   results
  at          :: (IndexVal m ~ Double, Mesh m) => a m -> Double -> Double
  -- | Tabulate function
  tabulateFun :: (IndexVal m ~ Double, Mesh m) => m -> (Double -> Double) -> a m
  -- | Use table of already evaluated function and mesh. Sizes of mesh
  --   and table must coincide
  tabulate    :: (IndexVal m ~ Double, Mesh m, G.Vector v Double) => m -> v Double -> a m

----------------------------------------------------------------

-- | Data for linear interpolation
data LinearInterp a = LinearInterp { linearInterpMesh  :: a
                                   , linearInterpTable :: U.Vector Double
                                   }
                      deriving (Show,Eq,Data,Typeable)

instance Mesh a => Indexable (LinearInterp a) where
  type IndexVal (LinearInterp a) = (IndexVal a, Double)
  size        (LinearInterp _    vec)   = size vec
  unsafeIndex (LinearInterp mesh vec) i = ( unsafeIndex mesh i
                                          , unsafeIndex vec  i
                                          )
  {-# INLINE size        #-}
  {-# INLINE unsafeIndex #-}

instance Interpolation LinearInterp where
  at = linearInterpolation
  tabulate mesh tbl
    | size mesh /= G.length tbl = error "Numeric.Tools.Interpolation.LinearInterp.tabulate: size of vector and mesh do not match"
    | otherwise                 = LinearInterp mesh (G.convert tbl)
  tabulateFun mesh f = LinearInterp mesh (U.generate (size mesh) (f . unsafeIndex mesh))

linearInterpolation :: (Mesh a, IndexVal a ~ Double) => LinearInterp a -> Double -> Double
linearInterpolation tbl@(LinearInterp mesh _) x = a + (x - xa) / (xb - xa) * (b - a)
  where
    n  = size mesh - 2
    i  = meshFindIndex mesh x
    i' | i < 0     = 0
       | i > n     = n
       | otherwise = i
    (xa,a) = unsafeIndex tbl  i
    (xb,b) = unsafeIndex tbl (i+1)
