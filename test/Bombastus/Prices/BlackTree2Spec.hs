{-# OPTIONS_GHC -Wno-missing-signatures #-}
{- |
   Module      : Bombastus.Prices.BlackTree2Spec
   Copyright   : Copyright (C) 2020 barsanges
   License     : GNU GPL, version 3

Test the module Bombastus.Prices.BlackTree2.
-}

module Bombastus.Prices.BlackTree2Spec (spec) where

import Test.Hspec
import Data.Time -- FIXME : we should rely only on Bombastus.DateTime
import Bombastus.Numerics
import Bombastus.Prices.Currency
import Bombastus.Prices.Market
import Bombastus.Prices.Product
import Bombastus.Prices.Provider
import Bombastus.Prices.BlackTree2

t0 = UTCTime { utctDay = fromGregorian 2018 6 9, utctDayTime = 0 }

t1 = UTCTime { utctDay = fromGregorian 2018 6 10, utctDayTime = 0 }

t2 = UTCTime { utctDay = fromGregorian 2018 6 11, utctDayTime = 0 }

mkt = Market "Power DE" (Currency "EUR")

year2019Base = Product mkt (Right (Year 2019)) Base

constantTree = mkTree t0 year2019Base 100 0 0

constantTree' = mkTree t0 year2019Base 100 (-0.1) 0

tree = mkTree t0 year2019Base 100 0.2 0

treeT0 = fromList [100]

treeT1 = fromList [100 * exp (0.2 * sqrt (1 / 365)),
                   100 * exp (-0.2 * sqrt (1 / 365))]

treeT2 = fromList [100 * exp (0.2 * 2 * sqrt (1 / 365)),
                   100,
                   100 * exp (-0.2 * 2 * sqrt (1 / 365))]

spec :: Spec
spec = do
  describe "test a degenerate Black tree (no volatility)" $ do
    it "at the first step" $ do
      getPrices constantTree year2019Base t0 `shouldBe` fromList [100]

    it "at the second step" $ do
      getPrices constantTree year2019Base t1 `shouldBe` fromList [100]

    it "at the third step" $ do
      getPrices constantTree year2019Base t2 `shouldBe` fromList [100]

    it "negative volatility is clipped at 0" $ do
      getPrices constantTree' year2019Base t2 `shouldBe` fromList [100]

    it "for regression at the first step" $ do
      compareXd (regress constantTree t0 (fromList [50])) (fromList [50]) `shouldBe` Nothing

    it "for regression at the second step" $ do
      compareXd (regress constantTree t1 (fromList [50])) (fromList [50]) `shouldBe` Nothing

    it "for regression at the third step" $ do
      compareXd (regress constantTree t2 (fromList [50])) (fromList [50]) `shouldBe` Nothing

  describe "test a regular Black tree" $ do
    it "at the first step" $ do
      compareXd treeT0 (getPrices tree year2019Base t0) `shouldBe` Nothing

    it "at the second step" $ do
      compareXd treeT1 (getPrices tree year2019Base t1) `shouldBe` Nothing

    it "at the third step" $ do
      compareXd treeT2 (getPrices tree year2019Base t2) `shouldBe` Nothing

    it "for regression at the first step" $ do
      compareXd (regress tree t0 treeT0) treeT0 `shouldBe` Nothing

    it "for regression between the second and first step" $ do
      compareXd (regress tree t0 treeT1) treeT0 `shouldBe` Nothing

    it "for regression between the third and first step" $ do
      compareXd (regress tree t0 treeT2) treeT0 `shouldBe` Nothing

    it "for regression between the third and first step" $ do
      compareXd (regress tree t1 treeT2) treeT1 `shouldBe` Nothing