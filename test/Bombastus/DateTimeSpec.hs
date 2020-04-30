{- |
   Module      : Bombastus.DateTimeSpec
   Copyright   : Copyright (C) 2020 barsanges
   License     : GNU GPL, version 3

Test the module Bombastus.DateTime.
-}

module Bombastus.DateTimeSpec (spec) where

import Test.Hspec
import Data.Time -- FIXME : we should rely only on Bombastus.DateTime
import Bombastus.DateTime

testDAH_01 = let start = UTCTime { utctDay = fromGregorian 2018 6 9, utctDayTime = 1203}
             in dah start 1
expectedDAH_01 = UTCTime { utctDay = fromGregorian 2018 6 10, utctDayTime = 0 }

testDAH_02 = let start = UTCTime { utctDay = fromGregorian 2020 4 17, utctDayTime = 544}
             in dah start 5
expectedDAH_02 = UTCTime { utctDay = fromGregorian 2020 4 22, utctDayTime = 0 }

testWAH_01 = let start = UTCTime { utctDay = fromGregorian 2020 5 27, utctDayTime = 81032}
             in wah start 1
expectedWAH_01 = UTCTime { utctDay = fromGregorian 2020 6 1, utctDayTime = 0 }

testWAH_02 = let start = UTCTime { utctDay = fromGregorian 2020 3 7, utctDayTime = 16597}
             in wah start 5
expectedWAH_02 = UTCTime { utctDay = fromGregorian 2020 4 6, utctDayTime = 0 }

testMAH_01 = let start = UTCTime { utctDay = fromGregorian 2019 4 1, utctDayTime = 54948}
             in mah start 1
expectedMAH_01 = UTCTime { utctDay = fromGregorian 2019 5 1, utctDayTime = 0 }

testMAH_02 = let start = UTCTime { utctDay = fromGregorian 1990 10 23, utctDayTime = 4896}
             in mah start 5
expectedMAH_02 = UTCTime { utctDay = fromGregorian 1991 3 1, utctDayTime = 0 }

testQAH_01 = let start = UTCTime { utctDay = fromGregorian 2029 8 16, utctDayTime = 65986}
             in qah start 1
expectedQAH_01 = UTCTime { utctDay = fromGregorian 2029 10 1, utctDayTime = 0 }

testQAH_02 = let start = UTCTime { utctDay = fromGregorian 2026 2 2, utctDayTime = 6549}
             in qah start 6
expectedQAH_02 = UTCTime { utctDay = fromGregorian 2027 7 1, utctDayTime = 0 }

testSAH_01 = let start = UTCTime { utctDay = fromGregorian 2012 11 11, utctDayTime = 68978}
             in sah start 1
expectedSAH_01 = UTCTime { utctDay = fromGregorian 2013 4 1, utctDayTime = 0 }

testSAH_02 = let start = UTCTime { utctDay = fromGregorian 2010 4 1, utctDayTime = 49870}
             in sah start 5
expectedSAH_02 = UTCTime { utctDay = fromGregorian 2012 10 1, utctDayTime = 0 }

testYAH_01 = let start = UTCTime { utctDay = fromGregorian 2017 2 27, utctDayTime = 65794}
             in yah start 1
expectedYAH_01 = UTCTime { utctDay = fromGregorian 2018 1 1, utctDayTime = 0 }

testYAH_02 = let start = UTCTime { utctDay = fromGregorian 2021 9 15, utctDayTime = 7965}
             in yah start 5
expectedYAH_02 = UTCTime { utctDay = fromGregorian 2026 1 1, utctDayTime = 0 }

spec :: Spec
spec = do
  describe "dah" $ do
    it "shifts a date from one day" $ do
      testDAH_01 `shouldBe` expectedDAH_01

    it "shifts a date from several days" $ do
      testDAH_02 `shouldBe` expectedDAH_02

    it "shifts a date from one week" $ do
      testWAH_01 `shouldBe` expectedWAH_01

    it "shifts a date from several weeks" $ do
      testWAH_02 `shouldBe` expectedWAH_02

    it "shifts a date from one month" $ do
      testMAH_01 `shouldBe` expectedMAH_01

    it "shifts a date from several months" $ do
      testMAH_02 `shouldBe` expectedMAH_02

    it "shifts a date from one quarter" $ do
      testQAH_01 `shouldBe` expectedQAH_01

    it "shifts a date from several quarters" $ do
      testQAH_02 `shouldBe` expectedQAH_02

    it "shifts a date from one season" $ do
      testSAH_01 `shouldBe` expectedSAH_01

    it "shifts a date from several seasons" $ do
      testSAH_02 `shouldBe` expectedSAH_02

    it "shifts a date from one year" $ do
      testYAH_01 `shouldBe` expectedYAH_01

    it "shifts a date from several years" $ do
      testYAH_02 `shouldBe` expectedYAH_02