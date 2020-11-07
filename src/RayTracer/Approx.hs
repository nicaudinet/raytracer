{-# LANGUAGE InstanceSigs #-}

module RayTracer.Approx where

class Approx a where
  approx :: a -> a -> Bool

instance Approx Double where
  approx :: Double -> Double -> Bool
  approx a b = abs (a - b) < 0.00001
