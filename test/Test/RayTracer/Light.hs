module Test.RayTracer.Light where

import Test.Approx
import Test.Tasty
import Test.Tasty.HUnit

import RayTracer.Color
import RayTracer.Light as L
import RayTracer.Tuple

tests :: TestTree
tests = testGroup "Light"
  [ testReflections
  , testPhongReflection
  , testLighting
  ]

testReflections :: TestTree
testReflections = testGroup "Reflections"
  [ testCase "Reflecting a vector approaching at 45" $
      reflect (vector 1 (-1) 0) (vector 0 1 0) @?~ vector 1 1 0
  , testCase "Reflecting a vector off a slanted surface" $
      let v = vector 0 (-1) 0
          n = vector (sqrt 2 / 2) (sqrt 2 / 2) 0
      in reflect v n @?~ vector 1 0 0
  ]

testPhongReflection :: TestTree
testPhongReflection = testGroup "Phong Reflection"
  [ testCase "A point light has a position and intensity" $ do
      let int = Color 1 1 1
          pos = point 0 0 0
          light = pointLight pos int
      L.position light @?~ pos
      L.intensity light @?~ int
  , testCase "The default material" $ do
      color defaultMaterial @?~ Color 1 1 1
      ambient defaultMaterial @?~ 0.1
      diffuse defaultMaterial @?~ 0.9
      specular defaultMaterial @?~ 0.9
      shininess defaultMaterial @?~ 200.0
  ]

testLighting :: TestTree
testLighting =
  let
    p = point 0 0 0
    m = defaultMaterial
  in
    testGroup "Lighting"
      [ testCase "Lighting with the eye between the light and the surface" $
          let eye = vector 0 0 (-1)
              normal = vector 0 0 (-1)
              light = pointLight (point 0 0 (-10)) (Color 1 1 1)
          in lighting m light p eye normal NotInShadow @?~ Color 1.9 1.9 1.9
      , testCase "Lighting with the eye between light and surface 45 degree offset" $
          let eye = vector 0 (sqrt 2 / 2) (sqrt 2 / 2)
              normal = vector 0 0 (-1)
              light = pointLight (point 0 0 (-10)) (Color 1 1 1)
          in lighting m light p eye normal NotInShadow @?~ Color 1 1 1
      , testCase "Lighting with eye opposite surface, light offset 45 degrees" $
          let eye = vector 0 0 (-1)
              normal = vector 0 0 (-1)
              light = pointLight (point 0 10 (-10)) (Color 1 1 1)
          in lighting m light p eye normal NotInShadow @?~ Color 0.7364 0.7364 0.7364
      , testCase "Lighting with eye in the path of the reflection vector" $
          let eye = vector 0 (-(sqrt 2) / 2) (-(sqrt 2) / 2)
              normal = vector 0 0 (-1)
              light = pointLight (point 0 10 (-10)) (Color 1 1 1)
          in lighting m light p eye normal NotInShadow @?~ Color 1.6364 1.6364 1.6364
      , testCase "Lighting with the light behind the surface" $
          let eye = vector 0 0 (-1)
              normal = vector 0 0 (-1)
              light = pointLight (point 0 0 10) (Color 1 1 1)
          in lighting m light p eye normal NotInShadow @?~ Color 0.1 0.1 0.1
      , testCase "Lighting with the surface in shadow" $
          let eye = vector 0 0 (-1)
              normal = vector 0 0 (-1)
              light = pointLight (point 0 0 (-10)) (Color 1 1 1)
          in lighting m light p eye normal InShadow @?~ Color 0.1 0.1 0.1
      ]
