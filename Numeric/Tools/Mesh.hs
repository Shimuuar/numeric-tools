module Numeric.Tools.Mesh (
    -- * Meshes
    Mesh(..)
  , validMeshIndex
  , (<!)
    -- ** Uniform mesh
  , UniformMesh
  , uniformMesh
  , uniformMeshStep
  ) where

----------------------------------------------------------------
-- Type class
----------------------------------------------------------------

-- | Class for one dimensional meshes. Mesh is ordered set of points.
class Mesh a where
  -- | Number of points in the mesh
  meshSize       :: a -> Int
  -- | Low bound of mesh
  meshLowerBound :: a -> Double
  -- | Upper bound of mesh 
  meshUpperBound :: a -> Double
  
  -- | Retrieve coordinate of point. No check are performed
  unsafeIndexMesh :: a -> Int -> Double
  -- | Find such index for value that 
  -- 
  -- > mesh <! i <= x && mesh <! i+1 > x
  --
  -- Will return invalid index if value is out of range.
  meshFindIndex :: a -> Double -> Int

-- | Check that index is valid
validMeshIndex :: Mesh a => a -> Int -> Bool 
validMeshIndex mesh i = i >= 0 && i < meshSize mesh
{-# INLINE validMeshIndex #-}

-- | Return point coordinate for mesh. Will throw exception if index
--   is out of bounds
(<!) :: Mesh a => a -> Int -> Double
mesh <! i 
  | i < 0 || i >= meshSize mesh = error "Numeric.Tool.Interpolation.Mesh: invalid index"
  | otherwise                   = unsafeIndexMesh mesh i



----------------------------------------------------------------
-- Uniform mesh
----------------------------------------------------------------

-- | Uniform mesh
data UniformMesh = UniformMesh { uniformMeshFrom :: Double
                               , uniformMeshStep :: Double
                               , uniformMeshSize :: Int
                               }

-- | Create uniform mesh 
uniformMesh :: Double           -- ^ Lower bound
            -> Double           -- ^ Upper bound
            -> Int              -- ^ Number of nodes
            -> UniformMesh
uniformMesh a b n
  | b <= a    = error "Numeric.Tool.Interpolation.Mesh.uniformMesh: wrong range"
  | n <  2    = error "Numeric.Tool.Interpolation.Mesh.uniformMesh: too few nodes"
  | otherwise = UniformMesh a ((b - a) / fromIntegral (n - 1)) n

instance Mesh UniformMesh where
  meshSize            = uniformMeshSize
  meshLowerBound      = uniformMeshFrom
  meshUpperBound (UniformMesh a da n) = a + da * fromIntegral (n - 1)

  unsafeIndexMesh (UniformMesh a da n) i = a + fromIntegral i * da
  meshFindIndex   (UniformMesh a da n) x = truncate $ (x - a) / da
