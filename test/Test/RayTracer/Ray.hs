module Test.RayTracer.Ray where

import Test.Approx
import Test.Tasty
import Test.Tasty.HUnit

import RayTracer.Matrix
import RayTracer.Ray
import RayTracer.Tuple

tests :: TestTree
tests = testGroup "Ray"
  [ testCase "Creating and querying a ray" $ do
      let orig = point 1 2 3
          dir = vector 4 5 6
          ray = Ray orig dir
      origin ray @?~ orig
      direction ray @?~ dir
  , testCase "Computing a point from a distance" $ do
      let ray = Ray (point 2 3 4) (vector 1 0 0)
      position ray 0 @?~ point 2 3 4
      position ray 1 @?~ point 3 3 4
      position ray (-1) @?~ point 1 3 4
      position ray 2.5 @?~ point 4.5 3 4
  , testCase "A ray intersects a sphere at two points" $
      let sphere = defaultSphere
          ray = Ray (point 0 0 (-5)) (vector 0 0 1)
      in intersect sphere ray @?~
          [ Intersection (Object sphere) 4.0
          , Intersection (Object sphere) 6.0
          ]
  , testCase "A ray intersects a sphere at a tangent" $
      let sphere = defaultSphere
          ray = Ray (point 0 1 (-5)) (vector 0 0 1)
      in intersect sphere ray @?~
          [ Intersection (Object sphere) 5.0
          , Intersection (Object sphere) 5.0
          ]
  , testCase "A ray misses a sphere" $
      let sphere = defaultSphere
          ray = Ray (point 0 2 (-5)) (vector 0 0 1)
      in intersect sphere ray @?~ []
  , testCase "A ray originates inside a sphere" $
      let sphere = defaultSphere
          ray = Ray (point 0 0 0) (vector 0 0 1)
      in intersect sphere ray @?~
          [ Intersection (Object sphere) (-1.0)
          , Intersection (Object sphere) 1.0
          ]
  , testCase "A sphere is behind a ray" $
      let sphere = defaultSphere
          ray = Ray (point 0 0 5) (vector 0 0 1)
      in intersect sphere ray @?~
          [ Intersection (Object sphere) (-6.0)
          , Intersection (Object sphere) (-4.0)
          ]
  , testCase "An intersection encapsulates t and object" $ do
      let intersection = Intersection (Object defaultSphere) 3.5
      time intersection @?~ 3.5
      object intersection @?~ Object defaultSphere
  , testCase "The hit, when all intersections have positive t" $
      let s = Object defaultSphere
          i1 = Intersection s 1
          i2 = Intersection s 2
      in hit [i1, i2] @?~ Just i1
  , testCase "The hit, when some intersections have negative t" $
      let s = Object defaultSphere
          i1 = Intersection s (-1)
          i2 = Intersection s 1
      in hit [i1, i2] @?~ Just i2
  , testCase "The hit, when all intersections have negative t" $
      let s = Object defaultSphere
          i1 = Intersection s (-2)
          i2 = Intersection s (-1)
      in hit [i1, i2] @?~ Nothing
  , testCase "The hit is always the lowest non-negative intersection" $
      let s = Object defaultSphere
          i1 = Intersection s 5
          i2 = Intersection s 7
          i3 = Intersection s (-3)
          i4 = Intersection s 2
      in hit [i1, i2, i3, i4] @?~ Just i4
  , testCase "Translating a ray" $ do
      let ray = Ray (point 1 2 3) (vector 0 1 0)
          m = translation 3 4 5
          ray2 = transform m ray
      origin ray2 @?~ point 4 6 8
      direction ray2 @?~ vector 0 1 0
  , testCase "Scaling a ray" $ do
      let ray = Ray (point 1 2 3) (vector 0 1 0)
          m = scaling 2 3 4
          ray2 = transform m ray
      origin ray2 @?~ point 2 6 12
      direction ray2 @?~ vector 0 3 0
  , testCase "A sphere's default transformation" $
      transformation defaultSphere @?~ identity
  , testCase "Changing a sphere's transformation" $
      let t = translation 2 3 4
          sphere = setTransformation t defaultSphere
      in transformation sphere @?~ t
  , testCase "Intersecting a scaled ray with a ray" $
      let ray = Ray (point 0 0 (-5)) (vector 0 0 1)
          sphere = setTransformation (scaling 2 2 2) defaultSphere
      in intersect sphere ray @?~
          [ Intersection (Object sphere) 3
          , Intersection (Object sphere) 7
          ]
  , testCase "Intersecting a translated ray with a ray" $
      let ray = Ray (point 0 0 (-5)) (vector 0 0 1)
          sphere = setTransformation (translation 5 0 0) defaultSphere
      in intersect sphere ray @?~ []
  ]
