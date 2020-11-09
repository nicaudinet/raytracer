{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RayTracer.Ray where

import Data.List (minimumBy)
import RayTracer.Approx
import RayTracer.Matrix
import RayTracer.Tuple

data Ray = Ray
  { origin :: Tuple
  , direction :: Tuple
  }
  deriving Show

newtype Sphere = Sphere { transformation :: Matrix }
  deriving stock Show
  deriving newtype Approx

newtype Object = Object Sphere
  deriving stock Show
  deriving newtype Approx

data Intersection = Intersection
  { object :: Object
  , time :: Double
  }
  deriving Show
  
instance Approx Intersection where
  approx (Intersection o1 t1) (Intersection o2 t2) =
    approx o1 o2 && approx t1 t2


position :: Ray -> Double -> Tuple
position (Ray orig dir) t = orig `add` (dir `mul` t)

intersect :: Sphere -> Ray -> [Intersection]
intersect sphere@(Sphere t) =
    map (Intersection (Object sphere))
  . intersectUnitSphere
  . transform (inverse t)

intersectUnitSphere :: Ray -> [Double]
intersectUnitSphere (Ray orig dir) =
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
      in [t1, t2]

hit :: [Intersection] -> Maybe Intersection
hit intersections =
  let is = filter ((>= 0) . time) intersections
  in
    if approx is []
    then Nothing
    else Just $ minimumBy (\i1 i2 -> compare (time i1) (time i2)) is

transform :: Matrix -> Ray -> Ray
transform matrix (Ray orig dir) = Ray (matrix |* orig) (matrix |* dir)

defaultSphere :: Sphere
defaultSphere = Sphere identity

setTransformation :: Matrix -> Sphere -> Sphere
setTransformation m (Sphere _sphere) = Sphere m
