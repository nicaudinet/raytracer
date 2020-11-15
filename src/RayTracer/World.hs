module RayTracer.World where

import GHC.Exts (sortWith)

import RayTracer.Matrix
import RayTracer.Tuple
import RayTracer.Color
import RayTracer.Sphere
import RayTracer.Light
import RayTracer.Ray as R

data World = World
  { objects :: [Object]
  , lightSource :: Maybe Light
  }
  deriving Show

emptyWorld :: World
emptyWorld = World [] Nothing

defaultWorld :: World
defaultWorld = World [Object sphere1, Object sphere2] (Just light)
  where
    material1 :: Material
    material1 = defaultMaterial
      { color = Color 0.8 1 0.6
      , diffuse = 0.7
      , specular = 0.2
      }

    sphere1 :: Sphere
    sphere1 = defaultSphere { material = material1 }

    transformation2 :: Matrix
    transformation2 = scaling 0.5 0.5 0.5

    sphere2 :: Sphere
    sphere2 = defaultSphere { transformation = transformation2 }

    light :: Light
    light = pointLight (point (-10) 10 (-10)) (Color 1 1 1)

intersectWorld :: Ray -> World -> [Intersection]
intersectWorld ray =
  sortWith time . concat . map (flip intersect ray . unObject) . objects

data Computation = Computation
  { compTime :: Double
  , compObject :: Object
  , compPoint :: Tuple
  , compEye :: Tuple
  , compNormal :: Tuple
  , compInside :: Bool
  }

prepareComputations :: Intersection -> Ray -> Computation
prepareComputations (Intersection obj t) ray =
  let
    pos = R.position ray t
    normal = normalAt obj pos
    eye = neg (direction ray)
    cond = dot normal eye < 0
  in
    Computation
      { compTime = t
      , compObject = obj
      , compPoint = pos
      , compEye = eye
      , compNormal = if cond then neg normal else normal
      , compInside = cond
      }

shadeHit :: World -> Computation -> Color
shadeHit world comp =
  case lightSource world of
    Nothing -> Color 0 0 0
    Just light ->
      lighting
        (material . unObject . compObject $ comp)
        light
        (compPoint comp)
        (compEye comp)
        (compNormal comp)

colorAt :: World -> Ray -> Color
colorAt world ray =
  case hit (intersectWorld ray world) of
    Nothing -> Color 0 0 0
    Just i -> shadeHit world (prepareComputations i ray) 

viewTransform :: Tuple -> Tuple -> Tuple -> Matrix
viewTransform from to up =
  let x = tupleX
      y = tupleY
      z = tupleZ
      forward = normalize (to `sub` from)
      left = forward `cross` (normalize up)
      trueUp = left `cross` forward
      orientation = Matrix
        [ [1,            0,            0,            0]
        , [0,   x    left ,   y    left ,   z    left ]
        , [0,   x  trueUp ,   y  trueUp ,   z  trueUp ]
        , [0, -(x forward), -(y forward), -(z forward)]
        ]
  in orientation |*| translation (-(x from)) (-(y from)) (-(z from))
