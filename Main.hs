module Main where

import RayTracer.Approx
import RayTracer.Tuple hiding (div)
import RayTracer.Color
import RayTracer.Canvas
import RayTracer.Matrix hiding (height, width)
import RayTracer.Ray

data Projectile = Projectile
  { pos :: Tuple
  , vel :: Tuple
  }

data Environment = Environment
  { gravity :: Tuple
  , wind :: Tuple
  }

tick :: Environment -> Projectile -> Projectile
tick env proj =
  Projectile
    { pos = (pos proj) `add` (vel proj)
    , vel = (vel proj) `add` (gravity env) `add` (wind env)
    }

run :: Environment -> Projectile -> [Tuple]
run env proj
  | tupleY (pos proj) <= 0 = []
  | otherwise = (pos proj) : run env (tick env proj)

coord :: Tuple -> (Double, Double)
coord (Tuple _w x y _z) = (x, y)

pointsToPPM :: Canvas -> [Tuple] -> PPM
pointsToPPM canvas points =
  canvasToPPM $
    foldr
      (\(x,y) -> writePixel (round x) (height canvas - round y) red)
      canvas
      (map coord points)

cannonball :: IO ()
cannonball =
  let
    projectile =
      Projectile (point 0 1 0) (normalize (vector 1 1.8 0) `mul` 11.25)
    environment = Environment (vector 0 (-0.1) 0) (vector (-0.01) 0 0)
    image = pointsToPPM (emptyCanvas 550 900) (run environment projectile)
  in savePPM "images/cannonball.ppm" image

clock :: IO ()
clock =
  let
    rotate n = rotationZ (n * (pi / 6))
    translate =  translation 250 250 0
    move n =  (translate |*| rotate n) |* point 200 0 0
    image = pointsToPPM (emptyCanvas 500 500) (map move [0 .. 11])
  in savePPM "images/clock.ppm" image

sphereShadow :: IO ()
sphereShadow = savePPM "images/sphere.ppm" (canvasToPPM image)
  where
    rayOrigin :: Tuple
    rayOrigin = point 0 0 (-5)

    wallZ :: Double
    wallZ = 10

    wallSize :: Double
    wallSize = 7

    canvasPixels :: Int
    canvasPixels = 200

    color :: Color
    color = red

    shape :: Sphere
    shape =
      let t = shearing 1 0 0 0 0 0 |*| scaling 0.5 1 1
      in setTransformation t defaultSphere

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
        if hit xs /~ Nothing
        then writePixel x y color canvas
        else canvas

main :: IO ()
main = sphereShadow
