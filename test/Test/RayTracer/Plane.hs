module Test.RayTracer.Plane where

import Test.Approx
import Test.Tasty
import Test.Tasty.HUnit

import RayTracer.Shape.Plane
import RayTracer.Ray
import RayTracer.Tuple

tests :: TestTree
tests = testGroup "Plane"
  [ testCase "The normal of a plane is constant everywhere" $ do
      localNormalAt defaultPlane (point 0 0 0) @?~ vector 0 1 0
      localNormalAt defaultPlane (point 10 0 (-10)) @?~ vector 0 1 0
      localNormalAt defaultPlane (point (-5) 0 150) @?~ vector 0 1 0
  , testCase "Intersect with a ray parallel to the plane" $ 
      let ray = Ray (point 0 10 0) (vector 0 0 1)
      in localIntersect defaultPlane ray @?~ []
  , testCase "Intersect with a coplanar ray" $
      let ray = Ray (point 0 0 0) (vector 0 0 1)
      in localIntersect defaultPlane ray @?~ []
  , testCase "A ray intersecting a plane from above" $
      let ray = Ray (point 0 1 0) (vector 0 (-1) 0)
      in localIntersect defaultPlane ray @?~
          [ Intersection (Object defaultPlane) 1 ]
  , testCase "A ray intersecting a plane from below" $
      let ray = Ray (point 0 (-1) 0) (vector 0 1 0)
      in localIntersect defaultPlane ray @?~
          [ Intersection (Object defaultPlane) 1 ]
  ]
