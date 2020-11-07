module Main where

import RayTracer.Tuple
import RayTracer.Color
import RayTracer.Canvas

data Projectile = Projectile
  { position :: Tuple
  , velocity :: Tuple
  }

data Environment = Environment
  { gravity :: Tuple
  , wind :: Tuple
  }

tick :: Environment -> Projectile -> Projectile
tick env proj =
  Projectile
    { position = (position proj) `add` (velocity proj)
    , velocity = (velocity proj) `add` (gravity env) `add` (wind env)
    }

run :: Environment -> Projectile -> [Tuple]
run env proj
  | tupleY (position proj) <= 0 = []
  | otherwise = (position proj) : run env (tick env proj)

main :: IO ()
main =
  let
    projectile =
      Projectile (point 0 1 0) (normalize (vector 1 1.8 0) `mul` 11.25)
    environment = Environment (vector 0 (-0.1) 0) (vector (-0.01) 0 0)
    h = 550
    canvas = emptyCanvas 900 h
    positions = run environment projectile
    coords = map (\(Tuple _ x y _) -> (x,y)) positions
    image = foldr (\(x,y) -> writePixel (round x) (h - round y) red) canvas coords
  in savePPM "images/cannonball.ppm" (canvasToPPM image)
      
