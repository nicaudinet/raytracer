module Test.RayTracer.Sphere where

import Test.Approx
import Test.Tasty
import Test.Tasty.HUnit

import RayTracer.Shape.Sphere
import RayTracer.Matrix
import RayTracer.Tuple
import RayTracer.Ray

tests :: TestTree
tests = testGroup "Sphere"
  [ testCase "The normal on a sphere at a point on the x axis" $
      localNormalAt (Object defaultSphere) (point 1 0 0) @?~ vector 1 0 0 
  , testCase "The normal on a sphere at a point on the y axis" $
      localNormalAt (Object defaultSphere) (point 0 1 0) @?~ vector 0 1 0 
  , testCase "The normal on a sphere at a point on the z axis" $
      localNormalAt (Object defaultSphere) (point 0 0 1) @?~ vector 0 0 1 
  , testCase "The normal on a sphere at a non-axial point" $
      let n = sqrt 3 / 3
      in localNormalAt (Object defaultSphere) (point n n n) @?~ vector n n n
  , testCase "The normal is a normalized vector" $
      let n = sqrt 3 / 3
          normal = localNormalAt (Object defaultSphere) (point n n n)
      in normal @?~ normalize normal
  , testCase "Computing the normal on a translated sphere" $
      let s = setTransform (translation 0 1 0) defaultSphere
      in normalAt (Object s) (point 0 1.70711 (-0.70711)) @?~
          vector 0 0.70711 (-0.70711)
  , testCase "Computing the normal on a transformed sphere" $
      let trans = (scaling 1 0.5 1) |*| (rotationZ (pi / 5))
          obj = Object $ setTransform trans defaultSphere
          n = (sqrt 2) / 2
      in normalAt obj (point 0 n (-n)) @?~ vector 0 0.97014 (-0.24254)
  ]
