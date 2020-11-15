module Test.RayTracer.Sphere where

import Test.Approx
import Test.Tasty
import Test.Tasty.HUnit

import RayTracer.Sphere
import RayTracer.Matrix
import RayTracer.Tuple
import RayTracer.Light

tests :: TestTree
tests = testGroup "Sphere"
  [ testSphereBasics
  , testSphereNormals
  ]

testSphereBasics :: TestTree
testSphereBasics = testGroup "Basics"
  [ testCase "A sphere's default transformation" $
      transformation defaultSphere @?~ identity
  , testCase "Changing a sphere's transformation" $
      let t = translation 2 3 4
          sphere = setTransformation t defaultSphere
      in transformation sphere @?~ t
  , testCase "A sphere has a default material" $
      material defaultSphere @?~ defaultMaterial
  , testCase "A sphere may be assigned a material" $
      let mat = defaultMaterial { ambient = 1 }
      in material (setMaterial mat defaultSphere) @?~ mat
  ]

testSphereNormals :: TestTree
testSphereNormals = testGroup "Normals"
  [ testCase "The normal on a sphere at a point on the x axis" $
      normalAt (Object defaultSphere) (point 1 0 0) @?~ vector 1 0 0 
  , testCase "The normal on a sphere at a point on the y axis" $
      normalAt (Object defaultSphere) (point 0 1 0) @?~ vector 0 1 0 
  , testCase "The normal on a sphere at a point on the z axis" $
      normalAt (Object defaultSphere) (point 0 0 1) @?~ vector 0 0 1 
  , testCase "The normal on a sphere at a non-axial point" $
      let n = sqrt 3 / 3
      in normalAt (Object defaultSphere) (point n n n) @?~ vector n n n
  , testCase "The normal is a normalized vector" $
      let n = sqrt 3 / 3
          normal = normalAt (Object defaultSphere) (point n n n)
      in normal @?~ normalize normal
  , testCase "Computing the normal on a translated sphere" $
      let sphere = setTransformation (translation 0 1 0) defaultSphere
      in normalAt (Object sphere) (point 0 1.70711 (-0.70711)) @?~
          vector 0 0.70711 (-0.70711)
  , testCase "Computing the normal on a transformed sphere" $
      let trans = (scaling 1 0.5 1) |*| (rotationZ (pi / 5))
          object = Object $ setTransformation trans defaultSphere
          n = (sqrt 2) / 2
      in normalAt object (point 0 n (-n)) @?~ vector 0 0.97014 (-0.24254)
  ]
