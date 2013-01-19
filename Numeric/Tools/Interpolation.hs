{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Module    : Numeric.Tools.Interpolation
-- Copyright : (c) 2011 Aleksey Khudyakov
-- License   : BSD3
--
-- Maintainer  : Aleksey Khudyakov <alexey.skladnoy@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- Function interpolation.
--
-- Sine interpolation using cubic splines:
--
-- >>> let tbl = cubicSpline $ tabulateFun (uniformMesh (0,10) 100) sin
-- >>> tbl `at` 1.786
-- 0.9769239849844867
module Numeric.Tools.Interpolation (
    -- * Type class
    Interpolation(..)
  , tabulate
  , tabulateFun
    -- * Linear interpolation
  , LinearInterp
  , linearInterp
    -- * Cubic splines
  , CubicSpline
  , cubicSpline
    -- * Reexport of mesh type
  , module Numeric.Tools.Mesh
    -- * Default methods
  , defaultInterpSize
  , defaultInterpIndex
  ) where

import Control.Monad.ST   (runST)
import Data.Data          (Data,Typeable)

import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as M

import Control.Monad.Numeric
import Numeric.Classes.Indexing
import Numeric.Tools.Mesh



----------------------------------------------------------------

-- | Type class for Interpolation algorithms. Since some algorithms
--   require some particular mesh type it's present as type class
--   parameter. Every algorithms should be instance of 'Indexable' as
--   well. Indexing should return pair @(x,y)@ for u'th mesh node.
class ( IndexVal (interp mesh) ~ (Double,Double), Indexable (interp mesh)
      , IndexVal mesh ~ Double, Mesh mesh
      ) => Interpolation interp mesh where
  -- | Interpolate function at some point. Function should not
  --   fail outside of mesh however it may and most likely will give
  --   nonsensical results
  at          :: interp mesh -> Double -> Double
  -- | Use table of already evaluated function and mesh. Sizes of mesh
  --   and table must coincide but it's not checked. Do not use this
  --   function use 'tabulate' instead.
  unsafeTabulate :: (G.Vector v Double) => mesh -> v Double -> interp mesh
  -- | Get mesh.
  interpolationMesh  :: interp mesh -> mesh
  -- | Get table of function values 
  interpolationTable :: interp mesh -> U.Vector Double
    
-- | Tabulate function.
tabulateFun :: (Interpolation i m) => m -> (Double -> Double) -> i m
tabulateFun mesh f = unsafeTabulate mesh $ U.generate (size mesh) (f . unsafeIndex mesh)
{-# INLINE tabulateFun #-}

-- | Use table of already evaluated function and mesh. Sizes of mesh
--   and table must coincide. 
tabulate :: (Interpolation i m, G.Vector v Double) => m -> v Double -> i m
{-# INLINE tabulate #-}
tabulate mesh tbl
  | size mesh /= G.length tbl = error "Numeric.Tools.Interpolation.tabulate: size of vector and mesh do not match"
  | otherwise                 = unsafeTabulate mesh tbl



----------------------------------------------------------------
-- Linear interpolation
----------------------------------------------------------------

-- | Data for linear interpolation
data LinearInterp mesh = LinearInterp
  { linearInterpMesh  :: mesh
  , linearInterpTable :: U.Vector Double
  }
  deriving (Show,Eq,Data,Typeable)

-- | Function used to fix types
linearInterp :: LinearInterp mesh -> LinearInterp mesh
linearInterp = id

instance (Mesh mesh, IndexVal mesh ~ Double) => Indexable (LinearInterp mesh) where
  type IndexVal (LinearInterp mesh) = (IndexVal mesh, Double)
  size        = defaultInterpSize
  unsafeIndex = defaultInterpIndex
  {-# INLINE size        #-}
  {-# INLINE unsafeIndex #-}

instance (Mesh mesh, IndexVal mesh ~ Double) => Interpolation LinearInterp mesh where
  at                      = linearInterpolation
  unsafeTabulate mesh tbl = LinearInterp mesh (G.convert tbl)
  interpolationMesh       = linearInterpMesh
  interpolationTable      = linearInterpTable

linearInterpolation :: (Mesh a, IndexVal a ~ Double) => LinearInterp a -> Double -> Double
linearInterpolation tbl@(LinearInterp mesh _) x = a + (x - xa) / (xb - xa) * (b - a)
  where
    i      = safeFindIndex mesh x
    (xa,a) = unsafeIndex tbl  i
    (xb,b) = unsafeIndex tbl (i+1)



----------------------------------------------------------------
-- Cubic splines
----------------------------------------------------------------

-- | Natural cubic splines
data CubicSpline a = CubicSpline { cubicSplineMesh   :: a
                                 , cubicSplineTable  :: U.Vector Double
                                 , _cubicSplineY2    :: U.Vector Double
                                 }
                   deriving (Eq,Show,Data,Typeable)

-- | Function used to fix types
cubicSpline :: CubicSpline a -> CubicSpline a 
cubicSpline = id

instance (Mesh mesh, IndexVal mesh ~ Double) => Indexable (CubicSpline mesh) where
  type IndexVal (CubicSpline mesh) = (IndexVal mesh, Double)
  size        = defaultInterpSize
  unsafeIndex = defaultInterpIndex
  {-# INLINE size        #-}
  {-# INLINE unsafeIndex #-}

instance (Mesh mesh, IndexVal mesh ~ Double) => Interpolation CubicSpline mesh where
  at (CubicSpline mesh ys y2) x = y
    where
    i  = safeFindIndex mesh x
    -- Table lookup
    xa = unsafeIndex mesh  i
    xb = unsafeIndex mesh (i+1)
    ya = unsafeIndex ys    i
    yb = unsafeIndex ys   (i+1)
    da = unsafeIndex y2    i
    db = unsafeIndex y2   (i+1)
    -- 
    h  = xb - xa
    a  = (xb - x ) / h
    b  = (x  - xa) / h
    y  = a * ya + b * yb 
       + ((a*a*a - a) * da + (b*b*b - b) * db) * (h * h) / 6
  ------
  unsafeTabulate mesh tbl = makeCubicSpline mesh (G.convert tbl)
  interpolationMesh       = cubicSplineMesh
  interpolationTable      = cubicSplineTable
      

-- These are natural cubic splines
makeCubicSpline :: (IndexVal a ~ Double, Mesh a) => a -> U.Vector Double -> CubicSpline a
makeCubicSpline xs ys = runST $ do
  let n = size ys
  y2 <- M.new n
  u  <- M.new n
  M.write y2 0 0.0
  M.write u  0 0.0
  -- Forward pass
  for 1 (n-1) $ \i -> do
    yVal <- M.read y2 (i-1)
    uVal <- M.read u  (i-1)
    let sig = delta xs i / delta xs (i+1)
        p   = sig * yVal + 2
        u'  = delta ys (i+1) / delta xs (i+1)  - delta ys i / delta xs i
    M.write y2 i $ (sig - 1) / p
    M.write u  i $ (6 * u' / (xs ! (i+1) - xs ! (i-1)) - sig * uVal) / p
  -- Backward pass
  M.write y2 (n-1) 0.0
  forGen (n-2) (>= 0) pred $ \i -> do
    uVal  <- M.read u   i
    yVal  <- M.read y2  i
    yVal1 <- M.read y2 (i+1)
    M.write y2 i $ yVal * yVal1 + uVal
  -- Done
  y2' <- G.unsafeFreeze y2
  return (CubicSpline xs ys y2')


----------------------------------------------------------------
-- Helpers

delta :: (Num (IndexVal a), Indexable a) => a -> Int -> IndexVal a
delta tbl i = (tbl ! i) - (tbl ! (i - 1))
{-# INLINE delta #-}

safeFindIndex :: Mesh a => a -> Double -> Int
safeFindIndex mesh x = 
  case meshFindIndex mesh x of
    i | i < 0     -> 0
      | i > n     -> n
      | otherwise -> i
    where
      n = size mesh - 2
{-# INLINE safeFindIndex #-}

-- | Default implementation of 'size' for interpolation algorithms.
defaultInterpSize :: Interpolation i m => i m -> Int
defaultInterpSize = U.length . interpolationTable
{-# INLINE defaultInterpSize #-}

-- | Default implementation of 'unsafeIndex' for interpolation algorithms.
defaultInterpIndex :: Interpolation i m => i m -> Int -> (Double, Double)
defaultInterpIndex tbl i = ( unsafeIndex (interpolationMesh  tbl) i
                           , unsafeIndex (interpolationTable tbl) i
                           )
{-# INLINE defaultInterpIndex #-}
