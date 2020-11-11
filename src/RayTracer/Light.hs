module RayTracer.Light where

import RayTracer.Approx
import RayTracer.Tuple
import RayTracer.Color

data Light = Light
  { position :: Tuple
  , intensity :: Color
  }

pointLight :: Tuple -> Color -> Light
pointLight = Light

data Material = Material
  { color :: Color
  , ambient :: Double
  , diffuse :: Double
  , specular :: Double
  , shininess :: Double
  }
  deriving Show

instance Approx Material where
  approx m1 m2 =
       approx (color m1) (color m2)
    && approx (ambient m1) (ambient m2)
    && approx (diffuse m1) (diffuse m2)
    && approx (specular m1) (specular m2)
    && approx (shininess m1) (shininess m2)

defaultMaterial :: Material
defaultMaterial = Material
  { color = Color 1 1 1
  , ambient = 0.1
  , diffuse = 0.9
  , specular = 0.9
  , shininess = 200.0
  }

reflect :: Tuple -> Tuple -> Tuple
reflect vec normal = vec `sub` (normal `mul` 2 `mul` (dot vec normal))

lighting :: Material -> Light -> Tuple -> Tuple -> Tuple -> Color
lighting m light p eye normal =
  ambientContrib `addColor` diffuseContrib `addColor` specularContrib
  where
    effectiveColor :: Color
    effectiveColor = (color m) `mulColor` (intensity light)

    lightV :: Tuple
    lightV = normalize ((position light) `sub` p)

    ambientContrib :: Color
    ambientContrib = effectiveColor `scaleColor` (ambient m)

    lightDotNormal :: Double
    lightDotNormal = lightV `dot` normal

    diffuseContrib :: Color
    diffuseContrib =
      if lightDotNormal < 0
      then black
      else effectiveColor `scaleColor` ((diffuse m) * lightDotNormal)

    specularContrib :: Color
    specularContrib =
      if lightDotNormal < 0
      then black
      else
        let reflectDotEye = (reflect (neg lightV) normal) `dot` eye
        in
          if reflectDotEye <= 0
          then black
          else
            scaleColor
              (intensity light)
              (specular m * (reflectDotEye ** (shininess m)))
