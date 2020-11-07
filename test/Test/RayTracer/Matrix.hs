module Test.RayTracer.Matrix where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Approx
import RayTracer.Tuple
import RayTracer.Matrix

tests :: TestTree
tests = testGroup "Matrix"
  [ testMatrixConstruction
  , testMatrixOperations
  , testMatrixInversion
  , testMatrixTransformations
  ]

testMatrixConstruction :: TestTree
testMatrixConstruction = testGroup "Construction"
  [ testCase "constructing and inspecting a 4x4 matrix" $ do
      let matrix = Matrix
            [ [   1,    2,    3,    4]
            , [ 5.5,  6.5,  7.5,  8.5]
            , [   9,   10,   11,   12]
            , [13.5, 14.5, 15.5, 16.5]
            ]
      extract 0 0 matrix @?= 1
      extract 0 3 matrix @?= 4
      extract 1 0 matrix @?= 5.5
      extract 1 2 matrix @?= 7.5
      extract 2 2 matrix @?= 11
      extract 3 0 matrix @?= 13.5
      extract 3 2 matrix @?= 15.5

  , testCase "a 2x2 matrix is representable" $ do
      let matrix = Matrix
            [ [-3,  5]
            , [ 1, -2]
            ]
      extract 0 0 matrix @?= -3
      extract 0 1 matrix @?= 5
      extract 1 0 matrix @?= 1
      extract 1 1 matrix @?= -2

  , testCase "a 3x3 matrix is representable" $ do
      let matrix = Matrix
            [ [-3,  5,  0]
            , [ 1, -2, -7]
            , [ 0,  1,  1]
            ]
      extract 0 0 matrix @?= -3
      extract 1 1 matrix @?= -2
      extract 2 2 matrix @?= 1
  ]

testMatrixOperations :: TestTree
testMatrixOperations = testGroup "Operations"
  [ testCase "Matrix equality with identical matrices" $
      let matrix = Matrix
            [ [1, 2, 3, 4]
            , [5, 6, 7, 8]
            , [9, 8, 7, 6]
            , [5, 4, 3, 2]
            ]
      in approx matrix matrix @?= True
  , testCase "Matrix equality with different matrices" $
      let matrix1 = Matrix
            [ [1, 2, 3, 4]
            , [5, 6, 7, 8]
            , [9, 8, 7, 6]
            , [5, 4, 3, 2]
            ]
          matrix2 = Matrix
            [ [2, 3, 4, 5]
            , [6, 7, 8, 9]
            , [8, 7, 6, 5]
            , [4, 3, 2, 1]
            ]
      in approx matrix1 matrix2 @?= False
  , testCase "Multiply two matrices" $
      let m1 = Matrix
            [ [1, 2, 3, 4]
            , [5, 6, 7, 8]
            , [9, 8, 7, 6]
            , [5, 4, 3, 2]
            ]
          m2 = Matrix
            [ [-2, 1, 2,  3]
            , [ 3, 2, 1, -1]
            , [ 4, 3, 6,  5]
            , [ 1, 2, 7,  8]
            ]
      in mulMatrix m1 m2 @?~ Matrix
          [ [20, 22,  50,  48]
          , [44, 54, 114, 108]
          , [40, 58, 110, 102]
          , [16, 26,  46,  42]
          ]
  , testCase "Matrix multiplied with a tuple" $
      let matrix = Matrix
            [ [1, 2, 3, 4]
            , [2, 4, 4, 2]
            , [8, 6, 4, 1]
            , [0, 0, 0, 1]
            ]
      in mulTuple matrix (Tuple 1 2 3 1) @?~ Tuple 18 24 33 1
  , testCase "Multiply a matrix by the identity matrix" $
      let matrix = Matrix
            [ [1, 2, 3, 4]
            , [2, 4, 4, 2]
            , [8, 6, 4, 1]
            , [0, 0, 0, 1]
            ]
      in mulMatrix matrix identity @?~ matrix
  , testCase "Transpose a matrix" $
      let matrix = Matrix
            [ [1, 2, 3, 4]
            , [2, 4, 4, 2]
            , [8, 6, 4, 1]
            , [0, 0, 0, 1]
            ]
      in transpose matrix @?~ Matrix
          [ [1, 2, 8, 0]
          , [2, 4, 6, 0]
          , [3, 4, 4, 0]
          , [4, 2, 1, 1]
          ]
  ]

testMatrixInversion :: TestTree
testMatrixInversion = testGroup "Inversion" $
  [ testCase "Determinant of a 2x2 matrix" $
      determinant (Matrix [[1,5],[-3,2]]) @?= 17
  , testCase "Submatrix of a 3x3 matrix is a 2x2 matrix" $
      let matrix = Matrix
            [ [ 1, 5,  0]
            , [-3, 2,  7]
            , [ 0, 6, -3]
            ]
      in submatrix 0 2 matrix @?~ Matrix
          [ [-3, 2]
          , [ 0, 6]
          ]
  , testCase "Submatrix of a 4x4 matrix is a 3x3 matrix" $
      let matrix = Matrix
            [ [-6, 1,  1, 6]
            , [-8, 5,  8, 6]
            , [-1, 0,  8, 2]
            , [-7, 1, -1, 1]
            ]
      in submatrix 2 1 matrix @?~ Matrix
          [ [-6,  1, 6]
          , [-8,  8, 6]
          , [-7, -1, 1]
          ]
  , testCase "Minor of a 3x3 matrix" $ do
      let matrix = Matrix
            [ [3,  5,  0]
            , [2, -1, -7]
            , [6, -1,  5]
            ]
      minor 1 0 matrix @?~ 25
      determinant (submatrix 1 0 matrix) @?~ 25
  , testCase "Cofactor of a 3x3 matrix" $ do
      let matrix = Matrix
            [ [3,  5,  0]
            , [2, -1, -7]
            , [6, -1,  5]
            ]
      minor 0 0 matrix @?~ -12
      cofactor matrix 0 0 @?~ -12
      minor 1 0 matrix @?~ 25
      cofactor matrix 1 0 @?~ -25
  , testCase "Determinant of a 3x3 matrix" $ do
      let matrix = Matrix
            [ [ 1, 2,  6]
            , [-5, 8, -4]
            , [ 2, 6,  4]
            ]
      cofactor matrix 0 0 @?~ 56
      cofactor matrix 0 1 @?~ 12
      cofactor matrix 0 2 @?~ -46
      determinant matrix @?~ -196
  , testCase "Determinant of a 4x4 matrix" $ do
      let matrix = Matrix
            [ [-2, -8,  3,  5]
            , [-3,  1,  7,  3]
            , [ 1,  2, -9,  6]
            , [-6,  7,  7, -9]
            ]
      cofactor matrix 0 0 @?~ 690
      cofactor matrix 0 1 @?~ 447
      cofactor matrix 0 2 @?~ 210
      cofactor matrix 0 3 @?~ 51
      determinant matrix @?~ -4071
  , testCase "Invertibility of an invertible matrix" $ do
      let matrix = Matrix
            [ [6,  4, 4,  4]
            , [5,  5, 7,  6]
            , [4, -9, 3, -7]
            , [9,  1, 7, -6]
            ]
      determinant matrix @?~ -2120
      invertible matrix @?= True
  , testCase "Invertibility of a not invertible matrix" $ do
      let matrix = Matrix
            [ [-4,  2, -2, -3]
            , [ 9,  6,  2,  6]
            , [ 0, -5,  1, -5]
            , [ 0,  0,  0,  0]
            ]
      determinant matrix @?~ 0
      invertible matrix @?= False
  , testCase "Inverse of a matrix" $ do
      let matrix = Matrix
            [ [-5,  2,  6, -8]
            , [ 1, -5,  1,  8]
            , [ 7,  7, -6, -7]
            , [ 1, -3,  7,  4]
            ]
          invMatrix = inverse matrix
      determinant matrix @?~ 532
      cofactor matrix 2 3 @?~ -160
      extract 3 2 invMatrix @?~ -160/532
      cofactor matrix 3 2 @?~ 105
      extract 2 3 invMatrix @?~ 105/532
      invMatrix @?~ Matrix
        [ [ 0.21805,  0.45113,  0.24060, -0.04511]
        , [-0.80827, -1.45677, -0.44361,  0.52068]
        , [-0.07895, -0.22368, -0.05263,  0.19737]
        , [-0.52256, -0.81391, -0.30075,  0.30639]
        ]
  , testCase "Multiply a product by its inverse" $
      let m1 = Matrix
            [ [ 3, -9,  7,  3]
            , [ 3, -8,  2, -9]
            , [-4,  4,  4,  1]
            , [-6,  5, -1,  1]
            ]
          m2 = Matrix
            [ [8,  2, 2, 2]
            , [3, -1, 7, 0]
            , [7,  0, 5, 4]
            , [6, -2, 0, 5]
            ]
      in (m1 |*| m2) |*| (inverse m2) @?~ m1
  ]

testMatrixTransformations :: TestTree
testMatrixTransformations = testGroup "Transformations"
  [ testCase "Multiplying a point by a translation matrix" $
      (translation 5 (-3) 2) |* (point (-3) 4 5) @?~ point 2 1 7
  , testCase "Multiplying a point by the inverse of a translation matrix" $
      (inverse $ translation 5 (-3) 2) |* (point (-3) 4 5) @?~ point (-8) 7 3
  , testCase "Translation does not affect vectors" $
      let vec = vector (-3) 4 5 in (translation 5 (-3) 2) |* vec @?~ vec
  , testCase "Multiplying a point by a scaling matrix" $
      (scaling 2 3 4) |* (point (-4) 6 8) @?~ point (-8) 18 32
  , testCase "Multiplying a vector by a scaling matrix" $
      (scaling 2 3 4) |* (vector (-4) 6 8) @?~ vector (-8) 18 32
  , testCase "Multiplying a vector by the inverse of a scaling matrix" $
      (inverse $ scaling 2 3 4) |* (vector (-4) 6 8) @?~ vector (-2) 2 2
  , testCase "Reflection is scaling by a negative value" $
      (scaling (-1) 1 1) |* (point 2 3 4) @?~ point (-2) 3 4
  , testCase "Rotating a point around the x axis" $ do
      (rotationX (pi / 4)) |* (point 0 1 0) @?~ point 0 (sqrt 2 / 2) (sqrt 2 / 2)
      (rotationX (pi / 2)) |* (point 0 1 0) @?~ point 0 0 1
  , testCase "Rotating a point around the y axis" $ do
      (rotationY (pi / 4)) |* (point 0 0 1) @?~ point (sqrt 2 / 2) 0 (sqrt 2 / 2)
      (rotationY (pi / 2)) |* (point 0 0 1) @?~ point 1 0 0
  , testCase "Rotating a point around the z axis" $ do
      (rotationZ (pi / 4)) |* (point 0 1 0) @?~ point (-(sqrt 2) / 2) (sqrt 2 / 2) 0
      (rotationZ (pi / 2)) |* (point 0 1 0) @?~ point (-1) 0 0
  ]
