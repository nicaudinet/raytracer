module Test.RayTracer.Color where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Approx
import RayTracer.Color

tests :: TestTree
tests = testGroup "Color"
  [ testColorBasic
  , testColorOperations
  ]

testColorBasic :: TestTree
testColorBasic =
  let color = Color 0.2 0.4 0.6
  in testGroup "red, green and blue"
    [ testCase "red" $ red color @?= 0.2
    , testCase "green" $ green color @?= 0.4
    , testCase "blue" $ blue color @?= 0.6
    ]

testColorOperations :: TestTree
testColorOperations = testGroup "Operations" $
  [ testCase "add two colors" $
      addColor (Color 0.2 0.4 0.6) (Color 0.1 0.3 0.5) @?~ Color 0.3 0.7 1.1 
  , testCase "subtract two colors" $
      subColor (Color 0.9 0.6 0.75) (Color 0.7 0.1 0.25) @?~ Color 0.2 0.5 0.5 
  , testCase "multiply a colors by a scalar" $
      scaleColor (Color 0.2 0.3 0.4) 2 @?~ Color 0.4 0.6 0.8 
  , testCase "multiply two colors" $
      mulColor (Color 0.9 1 0.4) (Color 1 0.2 0.1) @?~ Color 0.9 0.2 0.04
  ]
