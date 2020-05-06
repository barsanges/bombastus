{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE FlexibleInstances #-}
{- |
   Module      : Bombastus.NumericsSpec
   Copyright   : Copyright (C) 2020 barsanges
   License     : GNU GPL, version 3

Test the module Bombastus.Numerics.
-}

module Bombastus.NumericsSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Bombastus.Numerics

instance Arbitrary Xd where
  arbitrary = fmap fromList arbitrary

spec :: Spec
spec = do
  describe "zerosLike" $ do
    it "returns a vector of 0s" $ do
      zerosLike (fromList [-1, 2.5, -pi, 0]) `shouldBe` fromList [0, 0, 0, 0]

  describe "onesLike" $ do
    it "returns a vector of 1s" $ do
      onesLike (fromList [-1, 2.5, -pi, 0]) `shouldBe` fromList [1, 1, 1, 1]

  describe "almostEqual" $ do
    it "with a precision of 1" $ do
      almostEqualXd' 1 (fromList [1, 2, 3]) (fromList [0.5, 2.8, 3]) `shouldBe` True

    it "with a precision of 0.1" $ do
      almostEqualXd' 0.1 (fromList [1, 2, 3]) (fromList [0.5, 2.8, 3]) `shouldBe` False

    it "with a precision of 1e-9" $ do
      almostEqualXd' 1e-9 (fromList [1, 2, 3]) (fromList [1 + 1e-12, 2 - 1e-12, 3]) `shouldBe` True

    it "with different lengths" $ do
      almostEqualXd' 1e-9 (fromList [1, 2]) (fromList [1, 2, 3]) `shouldBe` False

    it "should always return True when applied to the same vector" $ property $
      \ a x -> almostEqualXd' (max a 1e-12) x x

  describe "addition" $ do
    it "of a vector and a scalar" $ do
      ((fromList [1, 2.5, pi]) .+ 1) `shouldBe` fromList [2, 3.5, pi + 1]

    it "of a scalar and a vector" $ do
      (1 +. (fromList [1, 2.5, pi])) `shouldBe` fromList [2, 3.5, pi + 1]

    it "of two vectors" $ do
      ((fromList [0, -1.8, 2.3e8]) .+. (fromList [1, 2.5, pi])) `shouldBe` fromList [1, 0.7, pi + 2.3e8]

    it "of a scalar and a vector is commutative" $ property $
      \ x a -> x .+ a == a +. x

    it "of two vectors is commutative" $ property $
      \ x y -> x .+. y == y .+. x

    it "of a vector and a scalar treats the scalar as a constant vector" $ property $
      \ x a -> x .+ a == x .+. (a +. zerosLike x)

    it "of two vectors has the length of the smallest vector" $ property $
      \ x y -> lengthXd (x .+. y) == min (lengthXd x) (lengthXd y)

    it "is associative" $ property $
      \ x y z -> almostEqualXd (x .+. (y .+. z)) ((x .+. y) .+. z)

  describe "substraction" $ do
    it "of a vector and a scalar" $ do
      ((fromList [-1, 2.5, pi]) .- 1) `shouldBe` fromList [-2, 1.5, pi - 1]

    it "of a scalar and a vector" $ do
      (1 -. (fromList [-1, 2.5, pi])) `shouldBe` fromList [2, -1.5, 1 - pi]

    it "of two vectors" $ do
      ((fromList [0, -1.8, 2.3e8]) .-. (fromList [1, 2.5, pi])) `shouldBe` fromList [-1, -4.3, 2.3e8 - pi]

    it "of a vector and itself is always null" $ property $
      \ x -> x .-. x == zerosLike x

    it "of two vectors has the length of the smallest vector" $ property $
      \ x y -> lengthXd (x .-. y) == min (lengthXd x) (lengthXd y)

    it "is associative" $ property $
      \ x y z -> almostEqualXd (x .-. y .-. z) ((x .-. y) .-. z)

  describe "multiplication" $ do
    it "of a vector and a scalar" $ do
      ((fromList [-1, 2.5, pi]) .* 0.43) `shouldBe` fromList [-0.43, 1.075, pi * 0.43]

    it "of a scalar and a vector" $ do
      (0.43 *. (fromList [-1, 2.5, pi])) `shouldBe` fromList [-0.43, 1.075, pi * 0.43]

    it "of two vectors" $ do
      ((fromList [0, -1.8, 2.3e8]) .*. (fromList [1, 2.5, pi])) `shouldBe` fromList [0, -4.5, 2.3e8 * pi]

    it "of a scalar and a vector is commutative" $ property $
      \ x a -> x .* a == a *. x

    it "of two vectors is commutative" $ property $
      \ x y -> x .*. y == y .*. x

    it "of a vector by an integer is an addition" $ property $
      \ x -> 2 *. x == x .+. x

    it "of two vectors has the length of the smallest vector" $ property $
      \ x y -> lengthXd (x .*. y) == min (lengthXd x) (lengthXd y)

    it "is associative" $ property $
      \ x y z -> almostEqualXd (x .*. (y .*. z)) ((x .*. y) .*. z)

    it "takes precedence over addition" $ property $
      \ x y z -> almostEqualXd (x .*. y .+. z) ((x .*. y) .+. z)

    it "takes precedence over addition" $ property $
      \ x y z -> almostEqualXd (x .+. y .*. z) (x .+. (y .*. z))

  describe "division" $ do
    it "of a vector and a scalar" $ do
      ((fromList [-1, 2.5, pi]) ./ 0.43) `shouldBe` fromList [-1 / 0.43, 2.5 / 0.43, pi / 0.43]

    it "of a scalar and a vector" $ do
      (0.43 /. (fromList [-1, 2.5, pi])) `shouldBe` fromList [-0.43, 0.172, 0.43 / pi]

    it "of two vectors" $ do
      ((fromList [0, -1.8, 2.3e8]) ./. (fromList [1, 2.5, pi])) `shouldBe` fromList [0, -0.72, 2.3e8 / pi]

    it "of two vectors has the length of the smallest vector" $ property $
      \ x y -> lengthXd (x ./. y) == min (lengthXd x) (lengthXd y)

  describe "max" $ do
    it "between two vectors" $ do
      (maxXd (fromList [-1, 2.5, pi]) (fromList [-0.5, -3e8, 3.14])) `shouldBe` fromList [-0.5, 2.5, pi]

    it "between a scalar and a vector" $ do
      (maxXd' 2 (fromList [-1, 2.5, pi])) `shouldBe` fromList [2, 2.5, pi]

    it "between two vectors is commutative" $ property $
      \ x y -> maxXd x y == maxXd y x

    it "of two vectors has the length of the smallest vector" $ property $
      \ x y -> lengthXd (maxXd x y) == min (lengthXd x) (lengthXd y)

  describe "min" $ do
    it "between two vectors" $ do
      (minXd (fromList [-1, 2.5, pi]) (fromList [-0.5, -3e8, 3.14])) `shouldBe` fromList [-1, -3e8, 3.14]

    it "between a scalar and a vector" $ do
      (minXd' 2 (fromList [-1, 2.5, pi])) `shouldBe` fromList [-1, 2, 2]

    it "between two vectors is commutative" $ property $
      \ x y -> minXd x y == minXd y x

    it "of two vectors has the length of the smallest vector" $ property $
      \ x y -> lengthXd (minXd x y) == min (lengthXd x) (lengthXd y)

  describe "positive value" $ do
    it "of a vector" $ do
      positiveXd (fromList [-1, 2.5, -pi, 0]) `shouldBe` fromList [0, 2.5, 0, 0]

  describe "clip" $ do
    it "a vector between two values" $ do
      clipXd 0 1 (fromList [0.5, 0, -2, 10, 1]) `shouldBe` fromList [0.5, 0, 0, 1, 1]

    it "is commutative with respect to the bounds" $ property $
      \ x a b -> clipXd a b x == clipXd b a x

  describe "abs" $ do
    it "of a vector" $ do
      absXd (fromList [-1, 2.5, -pi, 0]) `shouldBe` fromList [1, 2.5, pi, 0]

    it "of a difference is the difference between max and min" $ property $
      \ x y -> (maxXd x y) .-. (minXd x y) == absXd (x .-. y)

  describe "exp" $ do
    it "of a vector" $ do
      expXd (fromList [-1, 2.5, -pi, 0]) `shouldBe` fromList [exp (-1), exp 2.5, exp (-pi), 1]