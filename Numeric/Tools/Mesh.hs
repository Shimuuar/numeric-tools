{-# LANGUAGE TypeFamilies #-}
module Numeric.Tools.Mesh (
    -- * Meshes
    Mesh(..)
    -- ** Uniform mesh
  , UniformMesh
  , uniformMesh
  , uniformMeshStep
  ) where

import Numeric.Classes.Indexing



----------------------------------------------------------------
-- Type class
----------------------------------------------------------------

-- | Class for one dimensional meshes. Mesh is ordered set of points.
class Indexable a => Mesh a where
  -- | Low bound of mesh
  meshLowerBound :: a -> Double
  -- | Upper bound of mesh 
  meshUpperBound :: a -> Double
  
  -- | Find such index for value that 
  -- 
  -- > mesh <! i <= x && mesh <! i+1 > x
  --
  -- Will return invalid index if value is out of range.
  meshFindIndex :: a -> Double -> Int




----------------------------------------------------------------
-- Uniform mesh
----------------------------------------------------------------

-- | Uniform mesh
data UniformMesh = UniformMesh { uniformMeshFrom :: Double
                               , uniformMeshStep :: Double
                               , uniformMeshSize :: Int
                               }
                   deriving (Eq,Show)

-- | Create uniform mesh 
uniformMesh :: Double           -- ^ Lower bound
            -> Double           -- ^ Upper bound
            -> Int              -- ^ Number of nodes
            -> UniformMesh
uniformMesh a b n
  | b <= a    = error "Numeric.Tool.Interpolation.Mesh.uniformMesh: wrong range"
  | n <  2    = error "Numeric.Tool.Interpolation.Mesh.uniformMesh: too few nodes"
  | otherwise = UniformMesh a ((b - a) / fromIntegral (n - 1)) n


instance Indexable UniformMesh where
  type IndexVal UniformMesh = Double
  size                               = uniformMeshSize  
  unsafeIndex (UniformMesh a da n) i = a + fromIntegral i * da

instance Mesh UniformMesh where
  meshLowerBound                        = uniformMeshFrom
  meshUpperBound (UniformMesh a da n)   = a + da * fromIntegral (n - 1)
  meshFindIndex  (UniformMesh a da n) x = truncate $ (x - a) / da
