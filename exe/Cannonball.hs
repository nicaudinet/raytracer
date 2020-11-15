module Main where

import RayTracer.Tuple hiding (div)
import RayTracer.Color
import RayTracer.Canvas

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

main :: IO ()
main =
  let
    projectile =
      Projectile (point 0 1 0) (normalize (vector 1 1.8 0) `mul` 11.25)
    environment = Environment (vector 0 (-0.1) 0) (vector (-0.01) 0 0)
    image = pointsToPPM (emptyCanvas 550 900) (run environment projectile)
  in savePPM "images/cannonball.ppm" image
