module Main where

import RayTracer.Tuple hiding (div)
import RayTracer.Color
import RayTracer.Canvas
import RayTracer.Matrix hiding (height, width)
import RayTracer.Ray as R
import RayTracer.Sphere
import RayTracer.Light

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

sphere :: IO ()
sphere = savePPM "images/sphere.ppm" (canvasToPPM image)
  where
    rayOrigin :: Tuple
    rayOrigin = point 0 0 (-5)

    wallZ :: Double
    wallZ = 10

    wallSize :: Double
    wallSize = 7

    canvasPixels :: Int
    canvasPixels = 1000

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
              normal = normalAt shape p
              eye = neg (direction ray)
              m = material . unObject . object $ intersection
              c = lighting m light p eye normal
            in writePixel x y c canvas

main :: IO ()
main = sphere
