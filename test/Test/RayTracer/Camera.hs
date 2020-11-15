module Test.RayTracer.Camera where

import Test.Approx
import Test.Tasty
import Test.Tasty.HUnit

import RayTracer.Camera
import RayTracer.Canvas
import RayTracer.Color
import RayTracer.Matrix
import RayTracer.Tuple
import RayTracer.Ray
import RayTracer.World

tests :: TestTree
tests = testGroup "Camera"
  [ testCase "Constructing a camera" $ do
      let hsize = 160
          vsize = 120
          fieldOfView = pi / 2
          camera = defaultCamera hsize vsize fieldOfView
      cameraHsize camera @?= hsize
      cameraVsize camera @?= vsize
      cameraFieldOfView camera @?~ fieldOfView
      cameraTransform camera @?~ identity
  , testCase "The pixel size for a horizontal canvas" $
      let camera = defaultCamera 200 125 (pi / 2)
      in pixelSize camera @?~ 0.01
  , testCase "The pixel size for a vertical canvas" $
      let camera = defaultCamera 125 200 (pi / 2)
      in pixelSize camera @?~ 0.01
  , testCase "Constructing a ray through the center of the canvas" $ do
      let camera = defaultCamera 201 101 (pi / 2)
          ray = rayForPixel camera 100 50
      origin ray @?~ point 0 0 0
      direction ray @?~ vector 0 0 (-1) 
  , testCase "Constructing a ray through a corner of the canvas" $ do
      let camera = defaultCamera 201 101 (pi / 2)
          ray = rayForPixel camera 0 0
      origin ray @?~ point 0 0 0
      direction ray @?~ vector 0.66519 0.33259 (-0.66851)
  , testCase "Constructing a ray when the camera is transformed" $ do
      let trans = rotationY (pi / 4) |*| translation 0 (-2) 5
          camera = (defaultCamera 201 101 (pi / 2)) { cameraTransform = trans }
          ray = rayForPixel camera 100 50
      origin ray @?~ point 0 2 (-5)
      direction ray @?~ vector (sqrt 2 / 2) 0 (-(sqrt 2 / 2))
  , testCase "Rendering a world with a camera" $
      let world = defaultWorld
          from = point 0 0 (-5)
          to = point 0 0 0
          up = vector 0 1 0
          trans = viewTransform from to up
          camera = (defaultCamera 11 11 (pi / 2)) { cameraTransform = trans }
          image = render camera world
      in pixelAt 5 5 image @?~ Color 0.38066 0.47583 0.2855
  ]
