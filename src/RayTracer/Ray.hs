{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RayTracer.Ray where

import Data.List (minimumBy)

import RayTracer.Approx
import RayTracer.Matrix
import RayTracer.Tuple
import RayTracer.Light

-- * WithDefault helper datatype

data WithDefault a = Default | Value a
  deriving Show

instance Approx a => Approx (WithDefault a) where
  approx Default Default = True
  approx (Value a) (Value b) = approx a b
  approx _ _ = False

withDefault :: a -> WithDefault a -> a
withDefault def Default = def
withDefault _def (Value a) = a

-- * Shape typeclass

class Shape shape where
  transform :: shape -> Matrix
  setTransform :: Matrix -> shape -> shape
  material :: shape -> Material
  setMaterial :: Material -> shape -> shape
  localIntersect :: shape -> Ray -> [Intersection]
  localNormalAt :: shape -> Tuple -> Tuple

-- * Object existential type

type IsObject obj = (Approx obj, Show obj, Shape obj)
data Object = forall obj. IsObject obj => Object obj
deriving instance Show Object

instance Approx Object where
  approx o1 o2 =
    approx (transform o1) (transform o2) && approx (material o1) (material o2)

instance Shape Object where
  transform :: Object -> Matrix
  transform (Object shape) = transform shape

  setTransform :: Matrix -> Object -> Object
  setTransform mat (Object shape) = Object (setTransform mat shape)

  material :: Object -> Material
  material (Object shape) = material shape

  setMaterial :: Material -> Object -> Object
  setMaterial mat (Object shape) = Object (setMaterial mat shape)

  localIntersect :: Object -> Ray -> [Intersection]
  localIntersect (Object shape) = localIntersect shape

  localNormalAt :: Object -> Tuple -> Tuple
  localNormalAt (Object shape) = localNormalAt shape

-- * Ray datatype

data Ray = Ray
  { origin :: Tuple
  , direction :: Tuple
  }
  deriving Show

data Intersection = Intersection
  { object :: Object
  , time :: Double
  }
  deriving Show
  
instance Approx Intersection where
  approx (Intersection o1 t1) (Intersection o2 t2) = and
    [ approx (transform o1) (transform o2)
    , approx (material o1) (material o2)
    , approx t1 t2
    ]

position :: Ray -> Double -> Tuple
position (Ray orig dir) t = orig `add` (dir `mul` t)

-- * Shape intersections

intersect :: Object -> Ray -> [Intersection]
intersect obj
  = localIntersect obj
  . transformRay (inverse (transform obj))

normalAt :: Object -> Tuple -> Tuple
normalAt obj worldPoint =
  let
    localPoint = inverse (transform obj) |* worldPoint
    localNormal = localNormalAt obj localPoint
    worldNormal = transpose (inverse (transform obj)) |* localNormal
    worldNormalVector =
      -- This hack is necessary because the inverse transpose matrix might have
      -- messed up the w value of the tuple
      Tuple 0 (tupleX worldNormal) (tupleY worldNormal) (tupleZ worldNormal)
  in normalize worldNormalVector

hit :: [Intersection] -> Maybe Intersection
hit intersections =
  let is = filter ((>= 0) . time) intersections
  in
    if approx is []
    then Nothing
    else Just $ minimumBy (\i1 i2 -> compare (time i1) (time i2)) is

transformRay :: Matrix -> Ray -> Ray
transformRay matrix (Ray orig dir) = Ray (matrix |* orig) (matrix |* dir)
