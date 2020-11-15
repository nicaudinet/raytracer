module Main where

import RayTracer.Tuple hiding (div)
import RayTracer.Color
import RayTracer.Canvas
import RayTracer.Matrix hiding (height, width)
import RayTracer.Ray as R
import RayTracer.Sphere
import RayTracer.Light
import RayTracer.Camera hiding (pixelSize)
import RayTracer.World

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
              normal = normalAt (Object shape) p
              eye = neg (direction ray)
              m = material . unObject . object $ intersection
              c = lighting m light p eye normal
            in writePixel x y c canvas

threeSphereScene :: IO ()
threeSphereScene = savePPM "images/threeSphereScene.ppm" (canvasToPPM image)
  where
    wallMaterial :: Material
    wallMaterial = defaultMaterial { color = Color 1 0.9 0.9 , specular = 0 }

    floorSphere :: Sphere
    floorSphere = Sphere (scaling 10 0.01 10) wallMaterial

    leftWallTransform :: Matrix
    leftWallTransform
      =   translation 0 0 5
      |*| rotationY (-pi / 4)
      |*| rotationX (pi / 2)
      |*| scaling 10 0.01 10

    leftWall :: Sphere
    leftWall = Sphere leftWallTransform wallMaterial

    rightWallTransform :: Matrix
    rightWallTransform
      =   translation 0 0 5
      |*| rotationY (pi / 4)
      |*| rotationX (pi / 2)
      |*| scaling 10 0.01 10

    rightWall :: Sphere
    rightWall = Sphere rightWallTransform wallMaterial

    middleSphereMaterial :: Material
    middleSphereMaterial = defaultMaterial
      { color = Color 0.1 1 0.5
      , diffuse = 0.7
      , specular = 0.3
      }
    
    middleSphere :: Sphere
    middleSphere = Sphere (translation (-0.5) 1 0.5) middleSphereMaterial

    rightSphereTransform :: Matrix
    rightSphereTransform = translation 1.5 0.5 (-0.5) |*| scaling 0.5 0.5 0.5

    rightSphereMaterial :: Material
    rightSphereMaterial = defaultMaterial
      { color = Color 0.5 1 0.1
      , diffuse = 0.7
      , specular = 0.3
      }

    rightSphere :: Sphere
    rightSphere = Sphere rightSphereTransform rightSphereMaterial

    leftSphereTransform :: Matrix
    leftSphereTransform =
      translation (-1.5) 0.33 (-0.75) |*| scaling 0.33 0.33 0.33

    leftSphereMaterial :: Material
    leftSphereMaterial = defaultMaterial
      { color = Color 1 0.8 0.1
      , diffuse = 0.7
      , specular = 0.3
      }

    leftSphere :: Sphere
    leftSphere = Sphere leftSphereTransform leftSphereMaterial

    light :: Light
    light = pointLight (point (-10) 10 (-10)) (Color 1 1 1)

    cameraViewTransform :: Matrix
    cameraViewTransform =
      viewTransform (point 0 1.5 (-5)) (point 0 1 0) (vector 0 1 0)

    camera :: Camera
    camera = Camera
      { cameraHsize = 100
      , cameraVsize = 50
      , cameraFieldOfView = pi / 2
      , cameraTransform = cameraViewTransform
      }

    spheres :: [Sphere]
    spheres =
      [ floorSphere
      , leftWall
      , rightWall
      , middleSphere
      , rightSphere
      , leftSphere
      ]

    world :: World
    world = World (map Object spheres) (Just light)

    image :: Canvas
    image = render camera world

main :: IO ()
main = threeSphereScene
