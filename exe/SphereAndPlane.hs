module Main where

import RayTracer.Camera
import RayTracer.Canvas
import RayTracer.Color
import RayTracer.Light
import RayTracer.Matrix
import RayTracer.Shape.Sphere
import RayTracer.Shape.Plane
import RayTracer.Tuple
import RayTracer.World
import RayTracer.Ray

floorPlane :: Plane
floorPlane = defaultPlane

middleSphereMaterial :: Material
middleSphereMaterial = defaultMaterial
  { color = Color 0.1 1 0.5
  , diffuse = 0.7
  , specular = 0.3
  }

middleSphere :: Sphere
middleSphere = sphere (translation (-0.5) 1 0.5) middleSphereMaterial

rightSphereTransform :: Matrix
rightSphereTransform = translation 1.5 0.5 (-0.5) |*| scaling 0.5 0.5 0.5

rightSphereMaterial :: Material
rightSphereMaterial = defaultMaterial
  { color = Color 0.5 1 0.1
  , diffuse = 0.7
  , specular = 0.3
  }

rightSphere :: Sphere
rightSphere = sphere rightSphereTransform rightSphereMaterial

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
leftSphere = sphere leftSphereTransform leftSphereMaterial

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

objs :: [Object]
objs =
  [ Object floorPlane
  , Object middleSphere
  , Object rightSphere
  , Object leftSphere
  ]

world :: World
world = World objs (Just light)

image :: Canvas
image = render camera world

main :: IO ()
main = savePPM "images/sphereAndPlane.ppm" (canvasToPPM image)
