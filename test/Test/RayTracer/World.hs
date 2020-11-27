module Test.RayTracer.World where

import Test.Approx
import Test.Tasty
import Test.Tasty.HUnit

import RayTracer.World
import RayTracer.Light
import RayTracer.Tuple
import RayTracer.Matrix
import RayTracer.Color
import RayTracer.Shape.Sphere
import RayTracer.Ray

tests :: TestTree
tests = testGroup "World"
  [ testWorldBasics
  , testWorldIntersections
  , testWorldView
  , testWorldShadows
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
        sphere2 = Object (setTransform (scaling 0.5 0.5 0.5) defaultSphere)
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
      in shadeHit world comp @?~ Color 0.1 0.1 0.1
  , testCase "The color when a ray misses" $
      let ray = Ray (point 0 0 (-5)) (vector 0 1 0)
      in colorAt defaultWorld ray @?~ Color 0 0 0
  , testCase "The color when a ray hits" $
      let ray = Ray (point 0 0 (-5)) (vector 0 0 1)
      in colorAt defaultWorld ray @?~ Color 0.38066 0.47583 0.2855
  , testCase "The color with an intersection behind the ray" $
      let
        defOuterSphere = objects defaultWorld !! 0
        outerSphereMaterial = (material defOuterSphere) { ambient = 1 }
        outerSphere = setMaterial outerSphereMaterial defOuterSphere
      --
        defInnerSphere = objects defaultWorld !! 1
        innerSphereMaterial = (material defInnerSphere) { ambient = 1}
        innerSphere = setMaterial innerSphereMaterial defInnerSphere
      --
        world = World [outerSphere, innerSphere] (lightSource defaultWorld)
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

testWorldShadows :: TestTree
testWorldShadows = testGroup "Shadows"
  [ testCase "There is no shadow when nothing is collinear with point and light" $
      isShadowed defaultWorld (point 0 10 0) @?= NotInShadow
  , testCase "The shadow when an object is between the point and the light" $
      isShadowed defaultWorld (point 10 (-10) 10) @?= InShadow
  , testCase "There is no shadow when an object is behind the light" $
      isShadowed defaultWorld (point (-20) 20 (-20)) @?= NotInShadow
  , testCase "There is no shadow when an object is behind the point" $
      isShadowed defaultWorld (point (-2) 2 (-2)) @?= NotInShadow
  , testCase "shadeHit is given an intersection in shadow" $
      let
        light = pointLight (point 0 0 (-10)) (Color 1 1 1)
        sphere1 = Object defaultSphere
        sphere2 = Object (setTransform (translation 0 0 10) defaultSphere)
        world = World [sphere1, sphere2] (Just light)
        ray = Ray (point 0 0 5) (vector 0 0 1)
        i = Intersection (Object sphere2) 4
        comps = prepareComputations i ray
      in shadeHit world comps @?~ Color 0.1 0.1 0.1
  , testCase "The hit should offset the point" $ do
      let ray = Ray (point 0 0 (-5)) (vector 0 0 1)
          s = setTransform (translation 0 0 1) defaultSphere
          i = Intersection (Object s) 5
          comps = prepareComputations i ray
      tupleZ (compOverPoint comps) < (-epsilon) / 2 @?= True
      tupleZ (compPoint comps) > tupleZ (compOverPoint comps) @?= True
  ]
