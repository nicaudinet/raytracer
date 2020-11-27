module Main where

import Criterion.Main

import RayTracer.Camera
import RayTracer.Canvas
import RayTracer.Color
import RayTracer.Light
import RayTracer.Matrix
import RayTracer.Shape.Sphere
import RayTracer.Tuple
import RayTracer.World
import RayTracer.Ray

sphere :: Sphere
sphere =
  let
    trans = translation 1.5 0.5 (-0.5) |*| scaling 1.5 1.5 1.5
    mat =
      defaultMaterial
        { color = Color 1 0.3 0.1
        , diffuse = 0.7
        , specular = 0.3
        }
  in Sphere (Value trans) (Value mat)

light :: Light
light = pointLight (point (-10) 10 (-10)) (Color 1 1 1)

cameraViewTransform :: Matrix
cameraViewTransform =
  viewTransform (point 0 1.5 (-5)) (point 0 1 0) (vector 0 1 0)

camera :: Int -> Int -> Camera
camera hsize vsize = Camera
  { cameraHsize = hsize
  , cameraVsize = vsize
  , cameraFieldOfView = pi / 2
  , cameraTransform = cameraViewTransform
  }

world :: World
world = World [Object sphere] (Just light)

image :: (Int, Int) -> Canvas
image (hsize, vsize) = render (camera hsize vsize) world

main :: IO ()
main = do
  savePPM "images/threeSphereScene.ppm" (canvasToPPM $ image (100, 50))
  defaultMain
    [ bgroup "sphere"
        [ bench "10x5"   $ whnf image (10, 5)
        , bench "50x25"  $ whnf image (50, 25)
        , bench "80x40"  $ whnf image (80, 40)
        , bench "100x50" $ whnf image (100, 50)
        , bench "160x80" $ whnf image (160, 80)
        ]
    ]
