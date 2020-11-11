{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RayTracer.Sphere where

import RayTracer.Approx
import RayTracer.Light
import RayTracer.Tuple
import RayTracer.Matrix

data Sphere = Sphere
  { transformation :: Matrix
  , material :: Material
  }
  deriving stock Show

instance Approx Sphere where
  approx (Sphere t1 m1) (Sphere t2 m2) = approx t1 t2 && approx m1 m2

newtype Object = Object { unObject :: Sphere }
  deriving stock Show
  deriving newtype Approx

defaultSphere :: Sphere
defaultSphere = Sphere identity defaultMaterial

setTransformation :: Matrix -> Sphere -> Sphere
setTransformation m sphere = Sphere m (material sphere)

setMaterial :: Material -> Sphere -> Sphere
setMaterial mat sphere = sphere { material = mat }

-- The point is assumed to always be on the surface of the sphere
normalAt :: Sphere -> Tuple -> Tuple
normalAt sphere worldPoint =
  let
    objectPoint = inverse (transformation sphere) |* worldPoint
    objectNormal = objectPoint `sub` (point 0 0 0)
    worldNormal = transpose (inverse (transformation sphere)) |* objectNormal
    worldNormalVector = 
      -- This hack is necessary because the inverse transpose matrix might have
      -- messed up the w value of the tuple
      Tuple 0 (tupleX worldNormal) (tupleY worldNormal) (tupleZ worldNormal)
  in normalize worldNormalVector

