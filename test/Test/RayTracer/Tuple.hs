module Test.RayTracer.Tuple where

import Prelude hiding (div)
import Test.Approx
import Test.Tasty
import Test.Tasty.HUnit
import RayTracer.Tuple

tests :: TestTree
tests = testGroup "Tuples"
  [ testTupleConstructors
  , testTupleOperations
  , testTupleMagnitude
  , testTupleDotProduct
  ]

testTupleConstructors :: TestTree
testTupleConstructors = testGroup "Constructors"
  [ testCase "Tuple w, x, y, z" $ do
      let tuple = Tuple 1 2 3 4
      tupleW tuple @?= 1
      tupleX tuple @?= 2
      tupleY tuple @?= 3
      tupleZ tuple @?= 4
  , testCase "point" $
      point 4 (-4) 3 @?~ Tuple 1 4 (-4) 3
  , testCase "vector" $
      vector 4 (-4) 3 @?~ Tuple 0 4 (-4) 3
  ]

testTupleOperations :: TestTree
testTupleOperations = testGroup "Basic Operations"
  [ testCase "adding two tuples" $
      add (Tuple 1 3 (-2) 5) (Tuple 0 (-2) 3 1) @?~ Tuple 1 1 1 6
  , testCase "subtracting two points" $
      sub (point 3 2 1) (point 5 6 7) @?~ vector (-2) (-4) (-6)
  , testCase "subtracting a vector from a point" $
      sub (point 3 2 1) (vector 5 6 7) @?~ point (-2) (-4) (-6)
  , testCase "subtracting two vectors" $
      sub (vector 3 2 1) (vector 5 6 7) @?~ vector (-2) (-4) (-6)
  , testCase "subtracting a vector from a zero vector" $
      sub (vector 0 0 0) (vector 5 6 7) @?~ vector (-5) (-6) (-7)
  , testCase "negate a vector" $
      neg (vector 5 6 7) @?~ vector (-5) (-6) (-7)
  , testCase "multiply a tuple by a scalar" $
      mul (Tuple 1 (-2) 3 (-4)) 3.5 @?~ Tuple 3.5 (-7) 10.5 (-14)
  , testCase "multiply a tuple by a fraction" $
      mul (Tuple 1 (-2) 3 (-4)) 0.5 @?~ Tuple 0.5 (-1) 1.5 (-2)
  , testCase "divide a tuple by a scalar" $
      div (Tuple 1 (-2) 3 (-4)) 2 @?~ Tuple 0.5 (-1) 1.5 (-2)
  ]

testTupleMagnitude :: TestTree
testTupleMagnitude = testGroup "Magnitude"
  [ testCase "magnitude of vector(1, 0, 0)" $
      magnitude (vector 1 0 0) @?~ 1
  , testCase "magnitude of vector(0, 1, 0)" $
      magnitude (vector 0 1 0) @?~ 1
  , testCase "magnitude of vector(0, 0, 1)" $
      magnitude (vector 0 0 1) @?~ 1
  , testCase "magnitude of vector(1, 2, 3)" $
      magnitude (vector 1 2 3) @?~ sqrt 14
  , testCase "magnitude of vector(-1, -2, -3)" $
      magnitude (vector (-1) (-2) (-3)) @?~ sqrt 14
  , testCase "normalize vector(4,0,0)" $
      normalize (vector 4 0 0) @?~ vector 1 0 0
  , testCase "normalize vector(1,2,3)" $
      let vec = vector 1 2 3
          mag = magnitude vec
      in normalize vec @?~ vector (1 / mag) (2 / mag) (3 / mag)
  , testCase "magnitude of normalized vector" $
      magnitude (normalize (vector 1 2 3)) @?~ 1
  ]

testTupleDotProduct :: TestTree
testTupleDotProduct = testGroup "Dot Product"
  [ testCase "dot product of two vectors" $
      dot (vector 1 2 3) (vector 4 5 6) @?~ 32
  ]

testTupleCrossProduct :: TestTree
testTupleCrossProduct = testGroup "Cross Product"
  [ testCase "cross product of two vectors" $ do
      let a = (vector 1 2 3)
          b = (vector 2 3 4)
      cross a b @?~ vector (-1) 2 (-1)
      cross b a @?~ vector 1 (-2) 1
  ]
