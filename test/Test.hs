module Main where

import Test.Tasty
import qualified Test.RayTracer.Tuple as Tuple
import qualified Test.RayTracer.Color as Color
import qualified Test.RayTracer.Canvas as Canvas
import qualified Test.RayTracer.Matrix as Matrix

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ Tuple.tests
  , Color.tests
  , Canvas.tests
  , Matrix.tests
  ]
