{-# LANGUAGE InstanceSigs #-}

module Test.Approx
  ( assertApprox
  , (@?~)
  , module RayTracer.Approx
  ) where

import RayTracer.Approx
import Test.Tasty.HUnit

assertApprox :: (Show a, Approx a) => a -> a -> IO ()
assertApprox a b =
  if approx a b
  then pure ()
  else assertFailure (unwords [show a, "and", show b, "are not approximately equal"])

(@?~) :: (Show a, Approx a) => a -> a -> IO ()
actual @?~ expected = assertApprox actual expected

infix 1 @?~
