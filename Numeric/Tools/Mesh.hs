{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module    : Numeric.Tools.Mesh
-- Copyright : (c) 2011 Aleksey Khudyakov
-- License   : BSD3
--
-- Maintainer  : Aleksey Khudyakov <alexey.skladnoy@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- 1-dimensional meshes. Used by 'Numeric.Tools.Interpolation'.
--
module Numeric.Tools.Mesh (
    -- * Meshes
    Mesh(..)
    -- ** Uniform mesh
  , UniformMesh
  , uniformMeshStep
    -- *** Constructors
  , uniformMesh
  , uniformMeshN
  ) where

import Data.Data          (Data,Typeable)
import Numeric.Classes.Indexing



----------------------------------------------------------------
-- Type class
----------------------------------------------------------------

-- | Class for 1-dimensional meshes. Mesh is ordered set of
-- points. Each instance must guarantee that every next point is
-- greater that previous and there is at least 2 points in mesh.
--
-- Every mesh is instance of 'Indexable' and indexing should get n'th
-- mesh node.
class Indexable a => Mesh a where
  -- | Low bound of mesh
  meshLowerBound :: a -> Double
  -- | Upper bound of mesh
  meshUpperBound :: a -> Double

  -- | Find such index for value that
  --
  -- > mesh ! i <= x && mesh ! i+1 > x
  --
  -- Will return invalid index if value is out of range.
  meshFindIndex :: a -> Double -> Int




----------------------------------------------------------------
-- Uniform mesh
----------------------------------------------------------------

-- | Uniform mesh
data UniformMesh = UniformMesh { uniformMeshFrom :: Double
                               , uniformMeshStep :: Double 
                                 -- ^ Distance between points
                               , uniformMeshSize :: Int
                               }
                   deriving (Eq,Show,Data,Typeable)

-- | Create uniform mesh
uniformMesh :: (Double,Double)  -- ^ Lower and upper bound
            -> Int              -- ^ Number of points
            -> UniformMesh
uniformMesh (a,b) n
  | b <= a    = error "Numeric.Tool.Interpolation.Mesh.uniformMesh: bad range"
  | n <  2    = error "Numeric.Tool.Interpolation.Mesh.uniformMesh: too few points"
  | otherwise = UniformMesh a ((b - a) / fromIntegral (n - 1)) n

-- | Create uniform mesh
uniformMeshN :: Double  -- ^ Lower bound
             -> Double  -- ^ Mesh step
             -> Int     -- ^ Number of points
             -> UniformMesh
uniformMeshN a dx n
  | n  <  2   = error "Numeric.Tool.Interpolation.Mesh.uniformMeshStep: too few points"
  | dx <= 0   = error "Numeric.Tool.Interpolation.Mesh.uniformMeshStep: nonpositive step"
  | otherwise = UniformMesh a dx n


instance Indexable UniformMesh where
  type IndexVal UniformMesh = Double
  size                               = uniformMeshSize
  unsafeIndex (UniformMesh a da _) i = a + fromIntegral i * da

instance Mesh UniformMesh where
  meshLowerBound                        = uniformMeshFrom
  meshUpperBound (UniformMesh a da n)   = a + da * fromIntegral (n - 1)
  meshFindIndex  (UniformMesh a da _) x = truncate $ (x - a) / da
