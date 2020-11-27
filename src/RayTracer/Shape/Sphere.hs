{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RayTracer.Shape.Sphere
  ( Sphere(..)
  , defaultSphere
  , sphere
  ) where

import RayTracer.Approx
import RayTracer.Light
import RayTracer.Tuple
import RayTracer.Matrix
import RayTracer.Ray

data Sphere = Sphere
  { sphereTransform :: WithDefault Matrix
  , sphereMaterial :: WithDefault Material
  }
  deriving stock Show

instance Approx Sphere where
  approx (Sphere t1 m1) (Sphere t2 m2) = approx t1 t2 && approx m1 m2

instance Shape Sphere where
  transform :: Sphere -> Matrix
  transform = withDefault identity . sphereTransform

  setTransform :: Matrix -> Sphere -> Sphere
  setTransform t s = s { sphereTransform = Value t }

  material :: Sphere -> Material
  material = withDefault defaultMaterial . sphereMaterial

  setMaterial :: Material -> Sphere -> Sphere
  setMaterial m s = s { sphereMaterial = Value m }

  localIntersect :: Sphere -> Ray -> [Intersection]
  localIntersect s (Ray orig dir) =
    let
      sphereToRay = orig `sub` (point 0 0 0)
      a = dir `dot` dir
      b = 2 * (dir `dot` sphereToRay)
      c = (sphereToRay `dot` sphereToRay) - 1
      discriminant = (b ** 2) - 4 * a * c
    in
      if discriminant < 0
      then []
      else
        let t1 = (-b - sqrt discriminant) / (2 * a)
            t2 = (-b + sqrt discriminant) / (2 * a)
        in map (Intersection (Object s)) [t1, t2]

  -- The point is assumed to always be on the surface of the sphere
  localNormalAt :: Sphere -> Tuple -> Tuple
  localNormalAt _sphere localPoint = localPoint `sub` (point 0 0 0)

defaultSphere :: Sphere
defaultSphere = Sphere Default Default

sphere :: Matrix -> Material -> Sphere
sphere t m = Sphere (Value t) (Value m)
