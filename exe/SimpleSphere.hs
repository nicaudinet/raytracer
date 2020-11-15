module Main where

import RayTracer.Tuple
import RayTracer.Color
import RayTracer.Canvas
import RayTracer.Matrix
import RayTracer.Ray as R
import RayTracer.Sphere
import RayTracer.Light

rayOrigin :: Tuple
rayOrigin = point 0 0 (-5)

wallZ :: Double
wallZ = 10

wallSize :: Double
wallSize = 7

canvasPixels :: Int
canvasPixels = 100

shape :: Sphere
shape =
  let t = shearing 1 0 0 0 0 0 |*| scaling 1 1 1 |*| translation 0 0 5
      m = defaultMaterial { color = Color 0 0.2 1 }
  in setTransformation t (setMaterial m defaultSphere)

lightPosition :: Tuple
lightPosition = point (-10) 10 (-10)

lightColor :: Color
lightColor = Color 1 1 1

light :: Light
light = pointLight lightPosition lightColor

pixelSize :: Double
pixelSize = wallSize / fromIntegral canvasPixels

half :: Double
half = wallSize / 2

initCanvas :: Canvas
initCanvas = emptyCanvas canvasPixels canvasPixels

image :: Canvas
image = foldr goY initCanvas [0 .. canvasPixels - 1]

goY :: Int -> Canvas -> Canvas
goY y canvas =
  let worldY = half - pixelSize * (fromIntegral y)
  in foldr (goX y worldY) canvas [0 .. canvasPixels - 1]

goX :: Int -> Double -> Int -> Canvas -> Canvas
goX y worldY x canvas =
  let
    worldX = pixelSize * (fromIntegral x) - half
    worldPixel = point worldX worldY wallZ
    ray = Ray rayOrigin (normalize (worldPixel `sub` rayOrigin))
    xs = intersect shape ray
  in
    case hit xs of
      Nothing -> canvas
      Just intersection ->
        let
          p = R.position ray (time intersection)
          normal = normalAt (Object shape) p
          eye = neg (direction ray)
          m = material . unObject . object $ intersection
          c = lighting m light p eye normal
        in writePixel x y c canvas

main :: IO ()
main = savePPM "images/sphere.ppm" (canvasToPPM image)
