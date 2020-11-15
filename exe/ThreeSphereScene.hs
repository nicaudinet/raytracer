module Main where

import RayTracer.Camera
import RayTracer.Canvas
import RayTracer.Color
import RayTracer.Light
import RayTracer.Matrix
import RayTracer.Sphere
import RayTracer.Tuple
import RayTracer.World

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
main = savePPM "images/threeSphereScene.ppm" (canvasToPPM image)
