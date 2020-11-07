{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module RayTracer.Canvas where

import Data.List
import RayTracer.Color

newtype Canvas = Canvas [[Color]]

newtype PPM = PPM { unPPM :: String }

emptyCanvas :: Int -> Int -> Canvas
emptyCanvas w h = Canvas $ replicate h (replicate w black)

height :: Canvas -> Int
height (Canvas canvas) = length canvas

width :: Canvas -> Int
width (Canvas []) = 0
width (Canvas (w:_)) = (length w)

mapCanvas :: (Color -> a) -> Canvas -> [[a]]
mapCanvas f (Canvas canvas) = fmap (fmap f) canvas

pixelAt :: Int -> Int -> Canvas -> Color
pixelAt w h (Canvas canvas) = (canvas !! h) !! w

writePixel :: Int -> Int -> Color -> Canvas -> Canvas
writePixel w h color (Canvas canvas) =
  let (preRows, row:postRows) = splitAt h canvas
      (preCols, _:postCols) = splitAt w row
  in Canvas $ preRows <> [preCols <> [color] <> postCols] <> postRows

canvasToPPM :: Canvas -> PPM
canvasToPPM canvas@(Canvas pixels) =
  PPM $ unlines (header <> body)
  where
    header :: [String]
    header =
      [ "P3"
      , unwords [show (width canvas), show (height canvas)]
      , "255"
      ]

    scale255 :: Double -> Int
    scale255 = max 0 . min 255 . round . (* 255)

    showColor :: Color -> String
    showColor (Color r g b) = unwords (map (show . scale255) [r,g,b])

    splitLine :: String -> [String]
    splitLine = unfoldr splitAt70
      where
        findPreviousSpace :: Int -> String -> Int
        findPreviousSpace n str
          | str !! n == ' ' = n
          | otherwise = findPreviousSpace (n-1) str

        splitAt70 :: String -> Maybe (String, String)
        splitAt70 str
          | str == "" = Nothing
          | length str <= 70 = Just (str, "")
          | otherwise =
            let (prev, _space:post) = splitAt (findPreviousSpace 70 str) str
            in Just (prev, post)

    body :: [String]
    body = concat $ map (splitLine . unwords . map showColor) pixels
