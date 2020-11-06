module RayTracer.Color where

data Color = Color
  { red :: Double
  , green :: Double
  , blue :: Double
  }
  deriving (Show, Eq)

addColor :: Color -> Color -> Color
addColor (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1 + r2) (g1 + g2) (b1 + b2)

subColor :: Color -> Color -> Color
subColor (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1 - r2) (g1 - g2) (b1 - b2)

scaleColor :: Color -> Double -> Color
scaleColor (Color r g b) scalar = Color (r * scalar) (g * scalar) (b * scalar)

mulColor :: Color -> Color -> Color
mulColor (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1 * r2) (g1 * g2) (b1 * b2)
