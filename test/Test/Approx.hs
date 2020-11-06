{-# LANGUAGE InstanceSigs #-}

module Test.Approx where

import RayTracer.Color (Color(..))
import RayTracer.Tuple (Tuple(..))
import Test.Tasty.HUnit

class Approx a where
  approx :: a -> a -> Bool


instance Approx Double where
  approx :: Double -> Double -> Bool
  approx a b = abs (a - b) < 0.00001

instance Approx Tuple where
  approx (Tuple w1 x1 y1 z1) (Tuple w2 x2 y2 z2) =
    (approx w1 w2) && (approx x1 x2) && (approx y1 y2) && (approx z1 z2)

instance Approx Color where
  approx (Color r1 g1 b1) (Color r2 g2 b2) =
    (approx r1 r2) && (approx g1 g2) && (approx b1 b2)

assertApprox :: (Show a, Approx a) => a -> a -> IO ()
assertApprox a b =
  if approx a b
  then pure ()
  else assertFailure (unwords [show a, "and", show b, "are not approximately equal"])

(@?~) :: (Show a, Approx a) => a -> a -> IO ()
actual @?~ expected = assertApprox actual expected

infix 1 @?~
