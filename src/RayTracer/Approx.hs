{-# LANGUAGE InstanceSigs #-}

module RayTracer.Approx where

class Approx a where
  approx :: a -> a -> Bool
  (=~) :: a -> a -> Bool
  (=~) = approx
  (/~) :: a -> a -> Bool
  a /~ b = not (approx a b)

instance Approx Double where
  approx :: Double -> Double -> Bool
  approx a b = abs (a - b) < 0.00001

instance Approx a => Approx (Maybe a) where
  approx Nothing Nothing = True
  approx Nothing (Just _) = False
  approx (Just _) Nothing = False
  approx (Just a) (Just b) = approx a b

instance Approx a => Approx [a] where
  approx [] [] = True
  approx [] _  = False
  approx _  [] = False
  approx (a:as) (b:bs) = approx a b && approx as bs
