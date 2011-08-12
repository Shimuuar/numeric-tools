module Numeric.Tools.Interpolation (
    -- * Data types
    InterpData
  , interpMesh
  , interpData
  , unsafeIndexIData
    -- ** Constructors
  , interpolateFromVec
    -- *
  ) where

import qualified Data.Vector.Unboxed as U

import Numeric.Tools.Mesh

----------------------------------------------------------------

-- | Table for interpolation
data InterpData a = InterpData { interpMesh :: a
                               , interpData :: U.Vector Double
                               }
                    deriving (Show,Eq)

-- | Unsafely index interpolation table
unsafeIndexIData :: Mesh a => InterpData a -> Int -> (Double,Double)
unsafeIndexIData (InterpData mesh vec) i = ( unsafeIndexMesh mesh i
                                           , U.unsafeIndex   vec  i
                                           )
  
-- | Create table from mesh and vector of values.
interpolateFromVec :: Mesh a => a -> U.Vector Double -> InterpData a
interpolateFromVec mesh vec 
  | meshSize mesh /= U.length vec = error "Numeric.Tools.Interpolation.interpolateFromVec: size of vector and mesh do not match"
  | otherwise                     = InterpData mesh vec


----------------------------------------------------------------

-- | Linear interpolation. If value is out of range throw exception
linearInterpolation :: Mesh a => InterpData a -> Double -> Double
linearInterpolation tbl@(InterpData mesh _) x
  | validMeshIndex mesh i = workerLinearInterpolation tbl i x
  | otherwise             = error "Numeric.Tools.Interpolation.linearInterpolation: value is out of range"
  where
    i = meshFindIndex mesh x
  
-- | Linear interpolation
unsafeLinearInterpolation :: Mesh a => InterpData a -> Double -> Double
unsafeLinearInterpolation tbl@(InterpData mesh _) x =
  workerLinearInterpolation tbl (meshFindIndex mesh x) x


-- Worker function for linear interpolation
workerLinearInterpolation :: Mesh a => InterpData a -> Int -> Double -> Double
workerLinearInterpolation tbl i x = a + (x - xa) / (xb - xa) * (b - a)
  where
    (xa,a) = unsafeIndexIData tbl  i
    (xb,b) = unsafeIndexIData tbl (i+1)
