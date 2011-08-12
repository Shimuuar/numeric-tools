{-# LANGUAGE TypeFamilies #-}
module Numeric.Tools.Interpolation (
    -- * Data types
    InterpData
  , interpMesh
  , interpData
    -- ** Constructors
  , interpolateFromVec
    -- *
  ) where

import qualified Data.Vector.Unboxed as U

import Numeric.Classes.Indexing
import Numeric.Tools.Mesh

----------------------------------------------------------------

-- | Table for interpolation
data InterpData a = InterpData { interpMesh :: a
                               , interpData :: U.Vector Double
                               }
                    deriving (Show,Eq)

instance Mesh a => Indexable (InterpData a) where
  type IndexVal (InterpData a) = (IndexVal a, Double)
  size        (InterpData _    vec)   = size vec
  unsafeIndex (InterpData mesh vec) i = ( unsafeIndex mesh i
                                        , unsafeIndex vec  i
                                        )

  
-- | Create table from mesh and vector of values.
interpolateFromVec :: Mesh a => a -> U.Vector Double -> InterpData a
interpolateFromVec mesh vec 
  | size mesh /= size vec = error "Numeric.Tools.Interpolation.interpolateFromVec: size of vector and mesh do not match"
  | otherwise             = InterpData mesh vec


----------------------------------------------------------------

-- | Linear interpolation. If value is out of range throw exception
linearInterpolation :: (Mesh a, IndexVal a ~ Double) => InterpData a -> Double -> Double
linearInterpolation tbl@(InterpData mesh _) x
  | validIndex mesh i = workerLinearInterpolation tbl i x
  | otherwise         = error "Numeric.Tools.Interpolation.linearInterpolation: value is out of range"
  where
    i = meshFindIndex mesh x
  
-- | Linear interpolation
unsafeLinearInterpolation :: (Mesh a, IndexVal a ~ Double) => InterpData a -> Double -> Double
unsafeLinearInterpolation tbl@(InterpData mesh _) x =
  workerLinearInterpolation tbl (meshFindIndex mesh x) x


-- Worker function for linear interpolation
workerLinearInterpolation :: (Mesh a, IndexVal a ~ Double) => InterpData a -> Int -> Double -> Double
workerLinearInterpolation tbl i x = a + (x - xa) / (xb - xa) * (b - a)
  where
    (xa,a) = unsafeIndex tbl  i
    (xb,b) = unsafeIndex tbl (i+1)
