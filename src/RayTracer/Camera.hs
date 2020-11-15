{-# LANGUAGE RecordWildCards #-}

module RayTracer.Camera where

import RayTracer.Matrix
import RayTracer.Ray
import RayTracer.Tuple
import RayTracer.World
import RayTracer.Canvas

data Camera = Camera
  { cameraHsize :: Int
  , cameraVsize :: Int
  , cameraFieldOfView :: Double
  , cameraTransform :: Matrix
  }

defaultCamera :: Int -> Int -> Double -> Camera
defaultCamera hsize vsize fieldOfView = Camera
  { cameraHsize = hsize
  , cameraVsize = vsize
  , cameraFieldOfView = fieldOfView
  , cameraTransform = identity
  }

halfWidth :: Camera -> Double
halfWidth Camera{..} =
  let halfView = tan (cameraFieldOfView / 2)
      aspect = fromIntegral cameraHsize / fromIntegral cameraVsize
  in if aspect >= 1 then halfView else halfView * aspect

halfHeight :: Camera -> Double
halfHeight Camera{..} =
  let halfView = tan (cameraFieldOfView / 2)
      aspect = fromIntegral cameraHsize / fromIntegral cameraVsize
  in if aspect >= 1 then halfView / aspect else halfView

pixelSize :: Camera -> Double
pixelSize camera = (halfWidth camera) * 2 / fromIntegral (cameraHsize camera)

rayForPixel :: Camera -> Int -> Int -> Ray
rayForPixel camera px py =
  let
    xoffset = (fromIntegral px + 0.5) * pixelSize camera
    yoffset = (fromIntegral py + 0.5) * pixelSize camera
    worldX = (halfWidth camera) - xoffset
    worldY = (halfHeight camera) - yoffset
    invTrans = inverse (cameraTransform camera)
    pixel = invTrans |* point worldX worldY (-1)
    origin = invTrans |* point 0 0 0
    direction = normalize (pixel `sub` origin)
  in Ray origin direction

render :: Camera -> World -> Canvas
render camera world = foldImage write image
  where
    image :: Canvas
    image = emptyCanvas (cameraHsize camera) (cameraVsize camera)

    foldY :: (Int -> Canvas -> Canvas) -> Canvas -> Canvas
    foldY f canvas = foldr f canvas [ 0 .. (cameraVsize camera) - 1 ]

    foldX :: (Int -> Canvas -> Canvas) -> Canvas -> Canvas
    foldX f canvas = foldr f canvas [ 0 .. (cameraHsize camera) - 1 ]

    foldImage :: (Int -> Int -> Canvas -> Canvas) -> Canvas -> Canvas
    foldImage f canvas = foldY (\y -> foldX (f y)) canvas

    write :: Int -> Int -> Canvas -> Canvas
    write y x canvas =
      let ray = rayForPixel camera x y
          color = colorAt world ray
      in writePixel x y color canvas
