{-# OPTIONS_GHC -Wno-missing-signatures #-}
{- |
   Module      : Bombastus.Prices.ProductSpec
   Copyright   : Copyright (C) 2020 barsanges
   License     : GNU GPL, version 3

Test the module Bombastus.Prices.Product.
-}

module Bombastus.Prices.ProductSpec (spec) where

import Test.Hspec
import Data.Time -- FIXME : we should rely only on Bombastus.DateTime
import Bombastus.Prices.Currency
import Bombastus.Prices.Market
import Bombastus.Prices.Product

t = UTCTime { utctDay = fromGregorian 2018 6 9, utctDayTime = 0 }

mkt = Market "Power DE" (Currency "EUR")

dah3Base = Product mkt (Left (DAH 3)) Base

dah3Peak = Product mkt (Left (DAH 3)) Peak

dah3Offpeak = Product mkt (Left (DAH 3)) Offpeak

wah1Base = Product mkt (Left (WAH 1)) Base

wah1Peak = Product mkt (Left (WAH 1)) Peak

wah1Offpeak = Product mkt (Left (WAH 1)) Offpeak

mah1Base = Product mkt (Left (MAH 1)) Base

mah1Peak = Product mkt (Left (MAH 1)) Peak

mah1Offpeak = Product mkt (Left (MAH 1)) Offpeak

qah1Base = Product mkt (Left (QAH 1)) Base

qah1Peak = Product mkt (Left (QAH 1)) Peak

qah1Offpeak = Product mkt (Left (QAH 1)) Offpeak

sah1Base = Product mkt (Left (SAH 1)) Base

sah1Peak = Product mkt (Left (SAH 1)) Peak

sah1Offpeak = Product mkt (Left (SAH 1)) Offpeak

yah1Base = Product mkt (Left (YAH 1)) Base

yah1Peak = Product mkt (Left (YAH 1)) Peak

yah1Offpeak = Product mkt (Left (YAH 1)) Offpeak

week5Base = Product mkt (Right (Week 2019 5)) Base

week5Peak = Product mkt (Right (Week 2019 5)) Peak

week5Offpeak = Product mkt (Right (Week 2019 5)) Offpeak

jan2019Base = Product mkt (Right (Month 2019 1)) Base

jan2019Peak = Product mkt (Right (Month 2019 1)) Peak

jan2019Offpeak = Product mkt (Right (Month 2019 1)) Offpeak

quarter1Base = Product mkt (Right (Quarter 2019 1)) Base

quarter1Peak = Product mkt (Right (Quarter 2019 1)) Peak

quarter1Offpeak = Product mkt (Right (Quarter 2019 1)) Offpeak

summer2019Base = Product mkt (Right (Season 2019 Summer)) Base

summer2019Peak = Product mkt (Right (Season 2019 Summer)) Peak

summer2019Offpeak = Product mkt (Right (Season 2019 Summer)) Offpeak

year2019Base = Product mkt (Right (Year 2019)) Base

year2019Peak = Product mkt (Right (Year 2019)) Peak

year2019Offpeak = Product mkt (Right (Year 2019)) Offpeak

year2020Base = Product mkt (Right (Year 2020)) Base

freeStart = UTCTime { utctDay = fromGregorian 2018 8 15, utctDayTime = 12 * 3600 }

freeEnd = UTCTime { utctDay = fromGregorian 2018 8 17, utctDayTime = 17 * 3600 }

freeANone = Product mkt (Right (FreeA freeStart freeEnd)) None

freeABase = Product mkt (Right (FreeA freeStart freeEnd)) Base

freeAPeak = Product mkt (Right (FreeA freeStart freeEnd)) Peak

freeAOffpeak = Product mkt (Right (FreeA freeStart freeEnd)) Offpeak

spec :: Spec
spec = do
  describe "hoursInDelivery" $ do
    it "for a DAH base product" $ do
      hoursInDelivery dah3Base t `shouldBe` 24

    it "for a DAH peak product" $ do
      hoursInDelivery dah3Peak t `shouldBe` 12

    it "for a DAH offpeak product" $ do
      hoursInDelivery dah3Offpeak t `shouldBe` 12

    it "for a WAH base product" $ do
      hoursInDelivery wah1Base t `shouldBe` 168

    it "for a WAH peak product" $ do
      hoursInDelivery wah1Peak t `shouldBe` 60

    it "for a WAH offpeak product" $ do
      hoursInDelivery wah1Offpeak t `shouldBe` 108

    it "for a MAH base product" $ do
      hoursInDelivery mah1Base t `shouldBe` 744

    it "for a MAH peak product" $ do
      hoursInDelivery mah1Peak t `shouldBe` 264

    it "for a MAH offpeak product" $ do
      hoursInDelivery mah1Offpeak t `shouldBe` 480

    it "for a QAH base product" $ do
      hoursInDelivery qah1Base t `shouldBe` 2208

    it "for a QAH peak product" $ do
      hoursInDelivery qah1Peak t `shouldBe` 780

    it "for a QAH offpeak product" $ do
      hoursInDelivery qah1Offpeak t `shouldBe` 1428

    it "for a SAH base product" $ do
      hoursInDelivery sah1Base t `shouldBe` 4368

    it "for a SAH peak product" $ do
      hoursInDelivery sah1Peak t `shouldBe` 1560

    it "for a SAH offpeak product" $ do
      hoursInDelivery sah1Offpeak t `shouldBe` 2808

    it "for a YAH base product" $ do
      hoursInDelivery yah1Base t `shouldBe` 8760

    it "for a YAH peak product" $ do
      hoursInDelivery yah1Peak t `shouldBe` 3132

    it "for a YAH offpeak product" $ do
      hoursInDelivery yah1Offpeak t `shouldBe` 5628

    it "for a weekly base product" $ do
      hoursInDelivery week5Base t `shouldBe` 168

    it "for a weekly peak product" $ do
      hoursInDelivery week5Peak t `shouldBe` 60

    it "for a weekly offpeak product" $ do
      hoursInDelivery week5Offpeak t `shouldBe` 108

    it "for a monthly base product" $ do
      hoursInDelivery jan2019Base t `shouldBe` 744

    it "for a monthly peak product" $ do
      hoursInDelivery jan2019Peak t `shouldBe` 276

    it "for a monthly offpeak product" $ do
      hoursInDelivery jan2019Offpeak t `shouldBe` 468

    it "for a quarterly base product" $ do
      hoursInDelivery quarter1Base t `shouldBe` 2160

    it "for a quarterly peak product" $ do
      hoursInDelivery quarter1Peak t `shouldBe` 768

    it "for a quarterly offpeak product" $ do
      hoursInDelivery quarter1Offpeak t `shouldBe` 1392

    it "for a season base product" $ do
      hoursInDelivery summer2019Base t `shouldBe` 4392

    it "for a season peak product" $ do
      hoursInDelivery summer2019Peak t `shouldBe` 1572

    it "for a season offpeak product" $ do
      hoursInDelivery summer2019Offpeak t `shouldBe` 2820

    it "for a yearly base product" $ do
      hoursInDelivery year2019Base t `shouldBe` 8760

    it "for a yearly peak product" $ do
      hoursInDelivery year2019Peak t `shouldBe` 3132

    it "for a yearly offpeak product" $ do
      hoursInDelivery year2019Offpeak t `shouldBe` 5628

    it "for a yearly base product (leap year)" $ do
      hoursInDelivery year2020Base t `shouldBe` 8784

    it "for a free absolute product" $ do
      hoursInDelivery freeANone t `shouldBe` 53

    it "for a free absolute base product" $ do
      hoursInDelivery freeABase t `shouldBe` 53

    it "for a free absolute peak product" $ do
      hoursInDelivery freeAPeak t `shouldBe` 29

    it "for a free absolute offpeak product" $ do
      hoursInDelivery freeAOffpeak t `shouldBe` 24

-- FIXME: add a test for the property 'peak + offpeak = base'.