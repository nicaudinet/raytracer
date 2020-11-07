module Test.RayTracer.Canvas where

import RayTracer.Color
import RayTracer.Canvas
import Test.Approx
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Canvas"
  [ testCase "Empty canvas" $ do
      let canvas = emptyCanvas 20 10
      height canvas @?= 10
      width canvas @?= 20
      and (fmap and (mapCanvas (approx black) canvas)) @?= True
  , testCase "Writing pixels to canvas" $
      pixelAt 2 3 (writePixel 2 3 red (emptyCanvas 10 10)) @?~ red
  , testCase "Constructing the PPM header" $
      let header = take 3 . lines . unPPM . canvasToPPM $ emptyCanvas 5 3
      in header @?= ["P3","5 3","255"]
  , testCase "Constructing the PPM body" $
      let canvas =
              writePixel 0 0 (Color 1.5 0 0)
            . writePixel 2 1 (Color 0 0.5 0)
            . writePixel 4 2 (Color (-0.5) 0 1)
            $ emptyCanvas 5 3
          body = drop 3 . lines . unPPM . canvasToPPM $ canvas
      in body @?=
        [ "255 0 0 0 0 0 0 0 0 0 0 0 0 0 0"
        , "0 0 0 0 0 0 0 128 0 0 0 0 0 0 0"
        , "0 0 0 0 0 0 0 0 0 0 0 0 0 0 255"
        ]
  , testCase "Split long lines in the PPM body" $
      let canvas = Canvas $ mapCanvas (const (Color 1 0.8 0.6)) (emptyCanvas 10 2)
          body = drop 3 . lines . unPPM . canvasToPPM $ canvas
      in body @?=
        [ "255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204"
        , "153 255 204 153 255 204 153 255 204 153 255 204 153"
        , "255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204"
        , "153 255 204 153 255 204 153 255 204 153 255 204 153"
        ]
  , testCase "PPM file end with a newline" $
      let ppm = canvasToPPM (emptyCanvas 5 3)
      in last (unPPM ppm) @?= '\n'
  ]
