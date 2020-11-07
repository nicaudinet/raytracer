module RayTracer.Tuple where

import Prelude hiding (div)
import RayTracer.Approx

data Tuple = Tuple
  { tupleW :: Double
  , tupleX :: Double
  , tupleY :: Double
  , tupleZ :: Double
  }
  deriving (Show, Eq)

instance Approx Tuple where
  approx (Tuple w1 x1 y1 z1) (Tuple w2 x2 y2 z2) =
    (approx w1 w2) && (approx x1 x2) && (approx y1 y2) && (approx z1 z2)

point :: Double -> Double -> Double -> Tuple
point = Tuple 1.0

vector :: Double -> Double -> Double -> Tuple
vector = Tuple 0.0

add :: Tuple -> Tuple -> Tuple
add (Tuple w1 x1 y1 z1) (Tuple w2 x2 y2 z2) =
  Tuple (w1 + w2) (x1 + x2) (y1 + y2) (z1 + z2)

sub :: Tuple -> Tuple -> Tuple
sub (Tuple w1 x1 y1 z1) (Tuple w2 x2 y2 z2) =
  Tuple (w1 - w2) (x1 - x2) (y1 - y2) (z1 - z2)

neg :: Tuple -> Tuple
neg (Tuple w x y z) = Tuple (-w) (-x) (-y) (-z)

mul :: Tuple -> Double -> Tuple
mul (Tuple w x y z) a = Tuple (w * a) (x * a) (y * a) (z * a)

div :: Tuple -> Double -> Tuple
div (Tuple w x y z) a = Tuple (w / a) (x / a) (y / a) (z / a)

magnitude :: Tuple -> Double
magnitude (Tuple w x y z) =
  sqrt $ (w ** 2) + (x ** 2) + (y ** 2) + (z ** 2)

normalize :: Tuple -> Tuple
normalize t@(Tuple w x y z) = 
  let mag = magnitude t
  in Tuple (w / mag) (x / mag) (y / mag) (z / mag)

dot :: Tuple -> Tuple -> Double
dot (Tuple w1 x1 y1 z1) (Tuple w2 x2 y2 z2) =
  (w1 * w2) + (x1 * x2) + (y1 * y2) + (z1 * z2)

cross :: Tuple -> Tuple -> Tuple
cross (Tuple 0 x1 y1 z1) (Tuple 0 x2 y2 z2) =
  vector (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2)
cross _ _ = error "cross multiplication of non-vectors"
