module Main where

import Test.Tasty
import qualified Test.RayTracer.Tuple as Tuple
import qualified Test.RayTracer.Color as Color
import qualified Test.RayTracer.Canvas as Canvas
import qualified Test.RayTracer.Matrix as Matrix
import qualified Test.RayTracer.Ray as Ray
import qualified Test.RayTracer.Light as Light
import qualified Test.RayTracer.Sphere as Sphere
import qualified Test.RayTracer.World as World
import qualified Test.RayTracer.Camera as Camera
import qualified Test.RayTracer.Shape as Shape
import qualified Test.RayTracer.Plane as Plane

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ Tuple.tests
  , Color.tests
  , Canvas.tests
  , Matrix.tests
  , Ray.tests
  , Light.tests
  , Sphere.tests
  , World.tests
  , Camera.tests
  , Shape.tests
  , Plane.tests
  ]
