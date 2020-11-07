module RayTracer.Color where

import RayTracer.Approx

data Color = Color
  { redComponent :: Double
  , greenComponent :: Double
  , blueComponent :: Double
  }
  deriving (Show, Eq)

instance Approx Color where
  approx (Color r1 g1 b1) (Color r2 g2 b2) =
    (approx r1 r2) && (approx g1 g2) && (approx b1 b2)

-- * Operations

addColor :: Color -> Color -> Color
addColor (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1 + r2) (g1 + g2) (b1 + b2)

subColor :: Color -> Color -> Color
subColor (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1 - r2) (g1 - g2) (b1 - b2)

scaleColor :: Color -> Double -> Color
scaleColor (Color r g b) scalar = Color (r * scalar) (g * scalar) (b * scalar)

mulColor :: Color -> Color -> Color
mulColor (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1 * r2) (g1 * g2) (b1 * b2)

-- * Color shorthands

black :: Color
black = Color 0 0 0

white :: Color
white = Color 1 1 1

red :: Color
red = Color 1 0 0

green :: Color
green = Color 0 1 0

blue :: Color
blue = Color 0 0 1
