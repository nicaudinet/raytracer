module Main where

import RayTracer.Tuple
import RayTracer.Canvas
import RayTracer.Color
import RayTracer.Matrix hiding (height, width)
import System.Process (callProcess)

coord :: Tuple -> (Double, Double)
coord (Tuple _w x y _z) = (x, y)

pointsToPPM :: Canvas -> [Tuple] -> PPM
pointsToPPM canvas points =
  canvasToPPM $
    foldr
      (\(x,y) -> writePixel (round x) (height canvas - round y) red)
      canvas
      (map coord points)

main :: IO ()
main = do
  let
    rotate n = rotationZ (n * (pi / 6))
    translate =  translation 250 250 0
    move n =  (translate |*| rotate n) |* point 200 0 0
    image = pointsToPPM (emptyCanvas 500 500) (map move [0 .. 11])
    imageName = "images/clock.ppm" 
  savePPM imageName image
  callProcess "feh" [imageName]
