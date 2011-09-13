{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies       #-}
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
module Numeric.Tools.Interpolation (
    -- * Type class
    Interpolation(..)
    -- * Linear interpolation
  , LinearInterp
  , linearInterpMesh
  , linearInterpTable
    -- * Cubic splines
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

-- | Interpolation for arbitraty meshes
class Interpolation a where
  -- | Interpolate function at some point. Interpolation should not
  --   fail outside of mesh however it may and will give nonsensical
  --   results
  at          :: (IndexVal m ~ Double, Mesh m) => a m -> Double -> Double
  -- | Tabulate function
  tabulateFun :: (IndexVal m ~ Double, Mesh m) => m -> (Double -> Double) -> a m
  -- | Use table of already evaluated function and mesh. Sizes of mesh
  --   and table must coincide but it's not checked. Do not use this
  --   function use 'tabulate' instead.
  unsafeTabulate :: (IndexVal m ~ Double, Mesh m, G.Vector v Double) => m -> v Double -> a m

-- | Use table of already evaluated function and mesh. Sizes of mesh
--   and table must coincide. 
tabulate :: (Interpolation a, IndexVal m ~ Double, Mesh m, G.Vector v Double) => m -> v Double -> a m
tabulate mesh tbl
  | size mesh /= G.length tbl = error "Numeric.Tools.Interpolation.tabulate: size of vector and mesh do not match"
  | otherwise                 = unsafeTabulate mesh tbl
{-# INLINE tabulate #-}

----------------------------------------------------------------
-- Linear interpolation
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
  at                      = linearInterpolation
  tabulateFun    mesh f   = LinearInterp mesh (U.generate (size mesh) (f . unsafeIndex mesh))
  unsafeTabulate mesh tbl = LinearInterp mesh (G.convert tbl)

linearInterpolation :: (Mesh a, IndexVal a ~ Double) => LinearInterp a -> Double -> Double
linearInterpolation tbl@(LinearInterp mesh _) x = a + (x - xa) / (xb - xa) * (b - a)
  where
    i      = safeFindIndex mesh x
    (xa,a) = unsafeIndex tbl  i
    (xb,b) = unsafeIndex tbl (i+1)



----------------------------------------------------------------
-- Cubic splines
----------------------------------------------------------------

data CubicSpline a = CubicSpline { cubicSplineMesh   :: a
                                   , cubicSplineTable  :: U.Vector Double
                                   , cubicSplineY2     :: U.Vector Double
                                   }
                      deriving (Eq,Show,Data,Typeable)

instance Interpolation CubicSpline where
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
  tabulateFun    mesh f   = makeCubicSpline mesh (U.generate (size mesh) (f . unsafeIndex mesh))
  unsafeTabulate mesh tbl = makeCubicSpline mesh (G.convert tbl)
      

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
    do yVal <- M.read y2 (i-1)
       uVal <- M.read u  (i-1)
       let sig = delta xs i / delta xs (i+1)
           p   = sig * yVal + 2
           u'  = delta ys (i+1) / delta xs (i+1)  - delta ys i / delta xs i
       M.write y2 i $ (sig - 1) / p
       M.write u  i $ (6 * u' / (xs ! (i+1) - xs ! (i-1)) - sig * uVal) / p
  -- Backward pass
  M.write y2 (n-1) 0.0
  forGen (n-2) (>= 0) pred $ \i -> do
    do uVal  <- M.read u   i
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
