module Test.RayTracer.World where

import Test.Approx
import Test.Tasty
import Test.Tasty.HUnit

import RayTracer.World
import RayTracer.Light
import RayTracer.Tuple
import RayTracer.Matrix
import RayTracer.Color
import RayTracer.Sphere
import RayTracer.Ray

tests :: TestTree
tests = testGroup "World"
  [ testWorldBasics
  , testWorldIntersections
  , testWorldView
  ]

testWorldBasics :: TestTree
testWorldBasics = testGroup "Basics"
  [ testCase "Creating a world" $ do
      objects emptyWorld @?~ []
      lightSource emptyWorld @?~ Nothing
  , testCase "The default world" $ do
      let
        light = pointLight (point (-10) 10 (-10)) (Color 1 1 1)
        material1 = defaultMaterial
          { color = Color 0.8 1 0.6
          , diffuse = 0.7
          , specular = 0.2
          }
        sphere1 = Object (setMaterial material1 defaultSphere)
        sphere2 = Object (setTransformation (scaling 0.5 0.5 0.5) defaultSphere)
      objects defaultWorld @?~ [sphere1, sphere2]
      lightSource defaultWorld @?~ Just light
  ]

testWorldIntersections :: TestTree
testWorldIntersections = testGroup "Intersections"
  [ testCase "Intersect a world with a ray" $
      let ray = Ray (point 0 0 (-5)) (vector 0 0 1)
      in map time (intersectWorld ray defaultWorld) @?~ [4, 4.5, 5.5, 6]
  , testCase "Precomputing the state of an intersection" $ do
      let ray = Ray (point 0 0 (-5)) (vector 0 0 1)
          i = Intersection (Object defaultSphere) 4
          comp = prepareComputations i ray
      compTime comp @?~ time i
      compObject comp @?~ object i
      compPoint comp @?~ point 0 0 (-1)
      compEye comp @?~ vector 0 0 (-1)
      compNormal comp @?~ vector 0 0 (-1)
  , testCase "The hit, when an intersection occurs on the outside" $
      let ray = Ray (point 0 0 (-5)) (vector 0 0 1)
          i = Intersection (Object defaultSphere) 4
          comp = prepareComputations i ray
      in compInside comp @?= False
  , testCase "The hit, when an intersection occurs on the inside" $ do
      let ray = Ray (point 0 0 0) (vector 0 0 1)
          i = Intersection (Object defaultSphere) 1
          comp = prepareComputations i ray
      compInside comp @?= True
      compPoint comp @?~ point 0 0 1
      compEye comp @?~ vector 0 0 (-1)
      compNormal comp @?~ vector 0 0 (-1)
  , testCase "Shading an intersection" $
      let world = defaultWorld
          ray = Ray (point 0 0 (-5)) (vector 0 0 1)
          shape = head (objects world)
          i = Intersection shape 4
          comp = prepareComputations i ray
      in shadeHit world comp @?~ Color 0.38066 0.47583 0.2855
  , testCase "Shading an intersection from the inside" $
      let light = pointLight (point 0 0.25 0) (Color 1 1 1)
          world = defaultWorld { lightSource = Just light }
          ray = Ray (point 0 0 0) (vector 0 0 1)
          shape = objects world !! 1
          i = Intersection shape 0.5
          comp = prepareComputations i ray
      in shadeHit world comp @?~ Color 0.90498 0.90498 0.90498
  , testCase "The color when a ray misses" $
      let ray = Ray (point 0 0 (-5)) (vector 0 1 0)
      in colorAt defaultWorld ray @?~ Color 0 0 0
  , testCase "The color when a ray hits" $
      let ray = Ray (point 0 0 (-5)) (vector 0 0 1)
      in colorAt defaultWorld ray @?~ Color 0.38066 0.47583 0.2855
  , testCase "The color with an intersection behind the ray" $
      let defOuterSphere = unObject (objects defaultWorld !! 0)
          outerSphereMaterial = (material defOuterSphere) { ambient = 1 }
          outerSphere = defOuterSphere { material = outerSphereMaterial }
          defInnerSphere = unObject (objects defaultWorld !! 1)
          innerSphereMaterial = (material defInnerSphere) { ambient = 1}
          innerSphere = defInnerSphere { material = innerSphereMaterial }
          world =
            World
              [Object outerSphere, Object innerSphere]
              (lightSource defaultWorld)
          ray = Ray (point 0 0 0.75) (vector 0 0 (-1))
      in colorAt world ray @?~ color innerSphereMaterial
  ]

testWorldView :: TestTree
testWorldView = testGroup "View"
  [ testCase "The transformation matrix for the default orientation" $
      let from = point 0 0 0
          to = point 0 0 (-1)
          up = vector 0 1 0
      in viewTransform from to up @?~ identity
  , testCase "A view transformation matrix looking in positive z direction" $
      let from = point 0 0 0
          to = point 0 0 1
          up = vector 0 1 0
      in viewTransform from to up @?~ scaling (-1) 1 (-1)
  , testCase "The view transformation moves the world" $
      let from = point 0 0 8
          to = point 0 0 0
          up = vector 0 1 0
      in viewTransform from to up @?~ translation 0 0 (-8)
  , testCase "An arbitrary view transformation" $
      let from = point 1 3 2
          to = point 4 (-2) 8
          up = vector 1 1 0
      in viewTransform from to up @?~ Matrix
          [ [       1,  0.00000, 0.00000,  0.00000]
          , [-2.36643, -0.50709, 0.50709,  0.67612]
          , [-2.82843,  0.76772, 0.60609,  0.12122]
          , [       0, -0.35857, 0.59761, -0.71714]
          ]
  ]
