module RayTracer.Matrix where

import RayTracer.Approx
import RayTracer.Tuple

newtype Matrix = Matrix { unMatrix :: [[Double]] }
  deriving Show

instance Approx Matrix where
  approx m1 m2 =
    if height m1 /= height m2 || width m1 /= width m2
    then error "Comparing matrices of different sizes"
    else and $ zipWith approx (concat $ unMatrix m1) (concat $ unMatrix m2)


extract :: Int -> Int -> Matrix -> Double
extract r c (Matrix matrix) = (matrix !! r) !! c

height :: Matrix -> Int
height (Matrix matrix) = length matrix

width :: Matrix -> Int
width (Matrix []) = 0
width (Matrix (w:_)) = length w

rows :: Matrix -> [[Double]]
rows = unMatrix

row :: Matrix -> Int -> [Double]
row (Matrix m) n = m !! n

remove :: Int -> [a] -> [a]
remove n list
  | n < 0 || n >= length list = list
  | otherwise = let (pre, _:post) = splitAt n list in pre <> post

removeRow :: Int -> Matrix -> Matrix
removeRow n = Matrix . remove n . rows

cols :: Matrix -> [[Double]]
cols matrix = map (col matrix) [0 .. width matrix - 1]

col :: Matrix -> Int -> [Double]
col (Matrix m) n = fmap (!! n) m

removeCol :: Int -> Matrix -> Matrix
removeCol n = transpose . Matrix . remove n . cols

mulMatrix :: Matrix -> Matrix -> Matrix
mulMatrix m1 m2 =
  if width m1 /= height m2
  then error "Multiplying incompatible matrices"
  else Matrix
    [ [ sum (zipWith (*) (row m1 r) (col m2 c))
      | c <- [0 .. width m2 - 1]
      ]
    | r <- [0 .. height m1 - 1]
    ]

mulTuple :: Matrix -> Tuple -> Tuple
mulTuple matrix (Tuple w x y z) =
  let Matrix [[w'], [x'], [y'], [z']] =
        mulMatrix matrix (Matrix $ map (: []) [w,x,y,z])
  in Tuple w' x' y' z'

identity :: Matrix
identity = Matrix
  [ [1, 0, 0, 0]
  , [0, 1, 0, 0]
  , [0, 0, 1, 0]
  , [0, 0, 0, 1]
  ]

transpose :: Matrix -> Matrix
transpose = Matrix . cols

mapMatrix :: (Double -> Double) -> Matrix -> Matrix
mapMatrix f (Matrix matrix) = Matrix $ fmap (fmap f) matrix

mapIndex :: ((Int, Int) -> Double) -> Int -> Int -> Matrix
mapIndex f h w =
  Matrix
    [ [ f (r,c) | c <- [0 .. w - 1] ]
    | r <- [0 .. h - 1]
    ]

determinant :: Matrix -> Double
determinant (Matrix []) =
  error "Cannot calculate the determinant of an empty matrix"
determinant (Matrix [[n]]) = n
determinant matrix
  | height matrix < 2 || width matrix < 2 =
      error "Matrix is too small to calculate determinant"
  | otherwise =
      sum (zipWith (*) (row matrix 1) (row (cofactors matrix) 1))

submatrix :: Int -> Int -> Matrix -> Matrix
submatrix r c = removeCol c . removeRow r

minor :: Int -> Int -> Matrix -> Double
minor r c = determinant . submatrix r c

cofactor :: Matrix -> Int -> Int -> Double
cofactor matrix r c =
  minor r c matrix * (if odd (r + c) then -1 else 1)

cofactors :: Matrix -> Matrix
cofactors matrix =
  mapIndex (uncurry $ cofactor matrix) (height matrix) (width matrix)

invertible :: Matrix -> Bool
invertible = not  . approx 0.0 . determinant

inverse :: Matrix -> Matrix
inverse matrix =
  mapMatrix (/ determinant matrix) . transpose . cofactors $ matrix
