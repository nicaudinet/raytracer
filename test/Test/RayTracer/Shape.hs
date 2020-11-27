{-# LANGUAGE InstanceSigs #-}

module Test.RayTracer.Shape where

import Test.Approx
import Test.Tasty
import Test.Tasty.HUnit

import RayTracer.Tuple
import RayTracer.Matrix
import RayTracer.Ray
import RayTracer.Light

data TestShape = TestShape
  { testShapeTransform :: WithDefault Matrix
  , testShapeMaterial :: WithDefault Material
  }
  deriving Show

instance Approx TestShape where
  approx (TestShape t1 m1) (TestShape t2 m2) = approx t1 t2 && approx m1 m2

instance Shape TestShape where
  transform :: TestShape -> Matrix
  transform = withDefault identity . testShapeTransform

  setTransform :: Matrix -> TestShape -> TestShape
  setTransform t (TestShape _t m) = TestShape (Value t) m

  material :: TestShape -> Material
  material = withDefault defaultMaterial . testShapeMaterial

  setMaterial :: Material -> TestShape -> TestShape
  setMaterial m (TestShape t _m) = TestShape t (Value m)

  -- All we care about is that the ray has been transformed appropriately.
  -- Haxx: Encode the vector values into the time slot of the intersections.
  localIntersect :: TestShape -> Ray -> [Intersection]
  localIntersect ts (Ray (Tuple _ow ox oy oz) (Tuple _dw dx dy dz)) =
    map (Intersection (Object ts)) [ox, oy, oz, dx, dy, dz]

  localNormalAt :: TestShape -> Tuple -> Tuple
  localNormalAt _shape (Tuple _w x y z) = vector x y z

defaultTestShape :: TestShape
defaultTestShape = TestShape Default Default

tests :: TestTree
tests = testGroup "Shape"
  [ testCase "The default transformation" $
      transform defaultTestShape @?~ identity
  , testCase "Assigning the transformation" $
      let shape = setTransform (translation 2 3 4) defaultTestShape
      in transform shape @?~ translation 2 3 4
  , testCase "The default material" $
      material defaultTestShape @?~ defaultMaterial
  , testCase "Assigning the material" $
      let 
        m = defaultMaterial { ambient = 0.1 }
        shape = setMaterial m defaultTestShape
      in material shape @?~ m
  , testCase "Intersecting a scaled shape with a ray" $
      let
        ray = Ray (point 0 0 (-5)) (vector 0 0 1)
        shape = Object (setTransform (scaling 2 2 2) defaultTestShape)
        -- See the definition of localIntersect for TestShape for the hack
        result = map (Intersection shape) [0, 0, -2.5, 0, 0, 0.5]
      in intersect shape ray @?~ result
  , testCase "Intersecting a translated shape with a ray" $
      let
        ray = Ray (point 0 0 (-5)) (vector 0 0 1)
        shape = Object (setTransform (translation 5 0 0) defaultTestShape)
        -- See the definition of localIntersect for TestShape for the hack
        result = map (Intersection shape) [-5, 0, -5, 0, 0, 1]
      in intersect shape ray @?~ result
  , testCase "Computing the normal on the translated shape" $
      let
        shape = Object (setTransform (translation 0 1 0) defaultTestShape)
        normal = normalAt shape (point 0 1.70711 (-0.70711))
      in normal @?~ vector 0 0.70711 (-0.70711)
  , testCase "Computing the normal on the transformed shape" $
      let
        trans = scaling 1 0.5 1 |*| rotationZ (pi / 5)
        shape = Object (setTransform trans defaultTestShape)
        normal = normalAt shape (point 0 (sqrt 2 / 2) (-(sqrt 2 / 2)))
      in normal @?~ vector 0 0.97014 (-0.24254)
  ]
