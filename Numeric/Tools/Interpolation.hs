module Numeric.Tool.Interpolation (
  ) where

-- | Class for one dimensional meshes 
class Mesh a where
  -- | Find out size of mesh
  meshSize       :: a -> Int
  -- | Low bound of mesh
  meshLowerBound :: a -> Double
  -- | Upper bound of mesh 
  meshUpperBound :: a -> Double
  
  
  -- | Retrieve value at node. No check are performed
  unsafeMeshIndex :: a -> Int -> Double
  -- | Find such index for value that 
  -- 
  -- > mesh <! i <= x && mesh <! i+1 > x
  --
  -- Will return invalid index if value is out of range.
  meshFindIndex :: a -> Double -> Int

-- | Safer indexing for meshes. Will throw exception if index is out
--   of bounds
(<!) :: Mesh a => a -> Int -> Double
mesh <! i 
  | i < 0 || i >= meshSize mesh = error "Numeric.Tool.Interpolation.Mesh: invalid index"
  | otherwise                   = unsafeMeshIndex mesh i