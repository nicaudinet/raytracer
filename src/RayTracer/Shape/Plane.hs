{-# LANGUAGE InstanceSigs #-}

module RayTracer.Shape.Plane
  ( Plane(..)
  , defaultPlane
  , plane
  ) where

import RayTracer.Approx
import RayTracer.Ray
import RayTracer.Matrix
import RayTracer.Tuple
import RayTracer.Light

data Plane = Plane
  { planeTransform :: WithDefault Matrix
  , planeMaterial :: WithDefault Material
  }
  deriving Show

instance Approx Plane where
  approx (Plane t1 m1) (Plane t2 m2) = approx t1 t2 && approx m1 m2

instance Shape Plane where
  transform :: Plane -> Matrix
  transform = withDefault identity . planeTransform

  setTransform :: Matrix -> Plane -> Plane
  setTransform t (Plane _t m) = Plane (Value t) m

  material :: Plane -> Material
  material = withDefault defaultMaterial . planeMaterial

  setMaterial :: Material -> Plane -> Plane
  setMaterial m (Plane t _m) = Plane t (Value m)

  localIntersect :: Plane -> Ray -> [Intersection]
  localIntersect p ray =
    if abs (tupleY $ direction ray) < epsilon
    then []
    else
      let t = -(tupleY $ origin ray) / (tupleY $ direction ray)
      in [Intersection (Object p) t]

  localNormalAt :: Plane -> Tuple -> Tuple
  localNormalAt _plane _tuple = vector 0 1 0

defaultPlane :: Plane
defaultPlane = Plane Default Default

plane :: Matrix -> Material -> Plane
plane t m = Plane (Value t) (Value m)
