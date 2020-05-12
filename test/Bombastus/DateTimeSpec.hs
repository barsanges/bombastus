{-# OPTIONS_GHC -Wno-missing-signatures #-}
{- |
   Module      : Bombastus.DateTimeSpec
   Copyright   : Copyright (C) 2020 barsanges
   License     : GNU GPL, version 3

Test the module Bombastus.DateTime.
-}

module Bombastus.DateTimeSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Time -- FIXME : we should rely only on Bombastus.DateTime
import Bombastus.DateTime

instance Arbitrary Day where
  arbitrary = do
    y <- arbitrary
    m <- elements [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]
    d <- choose  (1, 31) -- FIXME: use chooseInt with QuickCheck >= 2.14
    return (fromGregorian y m d)

instance Arbitrary UTCTime where
  arbitrary = do
    d <- arbitrary
    s <- choose (0, 86401)
    return UTCTime { utctDay = d, utctDayTime = secondsToDiffTime s }

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

testNextPeakPeriod_01 = nextPeakPeriod $ UTCTime { utctDay = fromGregorian 2020 6 25, utctDayTime = 9 * 3600 }
expectedNextPeakPeriod_01 = (UTCTime { utctDay = fromGregorian 2020 6 25, utctDayTime = 9 * 3600 },
                             UTCTime { utctDay = fromGregorian 2020 6 25, utctDayTime = 20 * 3600 })

testNextPeakPeriod_02 = nextPeakPeriod $ UTCTime { utctDay = fromGregorian 2020 6 25, utctDayTime = 5 * 3600 }
expectedNextPeakPeriod_02 = (UTCTime { utctDay = fromGregorian 2020 6 25, utctDayTime = 8 * 3600 },
                             UTCTime { utctDay = fromGregorian 2020 6 25, utctDayTime = 20 * 3600 })

testNextPeakPeriod_03 = nextPeakPeriod $ UTCTime { utctDay = fromGregorian 2020 6 25, utctDayTime = 21 * 3600 }
expectedNextPeakPeriod_03 = (UTCTime { utctDay = fromGregorian 2020 6 26, utctDayTime = 8 * 3600 },
                             UTCTime { utctDay = fromGregorian 2020 6 26, utctDayTime = 20 * 3600 })

testNextPeakPeriod_04 = nextPeakPeriod $ UTCTime { utctDay = fromGregorian 2019 12 31, utctDayTime = 21 * 3600 }
expectedNextPeakPeriod_04 = (UTCTime { utctDay = fromGregorian 2020 1 1, utctDayTime = 8 * 3600 },
                             UTCTime { utctDay = fromGregorian 2020 1 1, utctDayTime = 20 * 3600 })

testNextPeakPeriod_05 = nextPeakPeriod $ UTCTime { utctDay = fromGregorian 2020 6 26, utctDayTime = 22 * 3600 }
expectedNextPeakPeriod_05 = (UTCTime { utctDay = fromGregorian 2020 6 29, utctDayTime = 8 * 3600 },
                             UTCTime { utctDay = fromGregorian 2020 6 29, utctDayTime = 20 * 3600 })

testNextOffpeakPeriod_01 = nextOffpeakPeriod $ UTCTime { utctDay = fromGregorian 2020 6 25, utctDayTime = 2 * 3600 }
expectedNextOffpeakPeriod_01 = (UTCTime { utctDay = fromGregorian 2020 6 25, utctDayTime = 2 * 3600 },
                                UTCTime { utctDay = fromGregorian 2020 6 25, utctDayTime = 8 * 3600 })

testNextOffpeakPeriod_02 = nextOffpeakPeriod $ UTCTime { utctDay = fromGregorian 2020 6 25, utctDayTime = 21 * 3600 }
expectedNextOffpeakPeriod_02 = (UTCTime { utctDay = fromGregorian 2020 6 25, utctDayTime = 21 * 3600 },
                                UTCTime { utctDay = fromGregorian 2020 6 26, utctDayTime = 8 * 3600 })

testNextOffpeakPeriod_03 = nextOffpeakPeriod $ UTCTime { utctDay = fromGregorian 2020 6 25, utctDayTime = 10 * 3600 }
expectedNextOffpeakPeriod_03 = (UTCTime { utctDay = fromGregorian 2020 6 25, utctDayTime = 20 * 3600 },
                                UTCTime { utctDay = fromGregorian 2020 6 26, utctDayTime = 8 * 3600 })

testNextOffpeakPeriod_04 = nextOffpeakPeriod $ UTCTime { utctDay = fromGregorian 2020 6 26, utctDayTime = 10 * 3600 }
expectedNextOffpeakPeriod_04 = (UTCTime { utctDay = fromGregorian 2020 6 26, utctDayTime = 20 * 3600 },
                                UTCTime { utctDay = fromGregorian 2020 6 29, utctDayTime = 8 * 3600 })

testNextOffpeakPeriod_05 = nextOffpeakPeriod $ UTCTime { utctDay = fromGregorian 2019 12 31, utctDayTime = 10 * 3600 }
expectedNextOffpeakPeriod_05 = (UTCTime { utctDay = fromGregorian 2019 12 31, utctDayTime = 20 * 3600 },
                                UTCTime { utctDay = fromGregorian 2020 1 1, utctDayTime = 8 * 3600 })

spec :: Spec
spec = do
  describe "dah" $ do
    it "shifts a date from one day" $ do
      testDAH_01 `shouldBe` expectedDAH_01

    it "shifts a date from several days" $ do
      testDAH_02 `shouldBe` expectedDAH_02

  describe "wah" $ do
    it "shifts a date from one week" $ do
      testWAH_01 `shouldBe` expectedWAH_01

    it "shifts a date from several weeks" $ do
      testWAH_02 `shouldBe` expectedWAH_02

  describe "mah" $ do
    it "shifts a date from one month" $ do
      testMAH_01 `shouldBe` expectedMAH_01

    it "shifts a date from several months" $ do
      testMAH_02 `shouldBe` expectedMAH_02

  describe "qah" $ do
    it "shifts a date from one quarter" $ do
      testQAH_01 `shouldBe` expectedQAH_01

    it "shifts a date from several quarters" $ do
      testQAH_02 `shouldBe` expectedQAH_02

  describe "sah" $ do
    it "shifts a date from one season" $ do
      testSAH_01 `shouldBe` expectedSAH_01

    it "shifts a date from several seasons" $ do
      testSAH_02 `shouldBe` expectedSAH_02

  describe "yah" $ do
    it "shifts a date from one year" $ do
      testYAH_01 `shouldBe` expectedYAH_01

    it "shifts a date from several years" $ do
      testYAH_02 `shouldBe` expectedYAH_02

  describe "nextPeakPeriod" $ do
    it "may have already started" $ do
      testNextPeakPeriod_01 `shouldBe` expectedNextPeakPeriod_01

    it "may start later the same day" $ do
      testNextPeakPeriod_02 `shouldBe` expectedNextPeakPeriod_02

    it "may start the following day" $ do
      testNextPeakPeriod_03 `shouldBe` expectedNextPeakPeriod_03

    it "may start the following year" $ do
      testNextPeakPeriod_04 `shouldBe` expectedNextPeakPeriod_04

    it "may start the following week" $ do
      testNextPeakPeriod_05 `shouldBe` expectedNextPeakPeriod_05

  describe "nextOffpeakPeriod" $ do
    it "may have already started" $ do
      testNextOffpeakPeriod_01 `shouldBe` expectedNextOffpeakPeriod_01

    it "may have already started (2)" $ do
      testNextOffpeakPeriod_02 `shouldBe` expectedNextOffpeakPeriod_02

    it "may start later the samed day" $ do
      testNextOffpeakPeriod_03 `shouldBe` expectedNextOffpeakPeriod_03

    it "may end after the weekend" $ do
      testNextOffpeakPeriod_04 `shouldBe` expectedNextOffpeakPeriod_04

    it "may end the following year" $ do
      testNextOffpeakPeriod_05 `shouldBe` expectedNextOffpeakPeriod_05

  describe "properties" $ do
    it "quarters start at the beginning of a month" $ property $
      \ x i -> (qah x i) `elem` [mah x j | j <- [3 * i - 2, 3 * i - 1, 3 * i]]

    it "seasons start at the beginning of a quarter" $ property $
      \ x i -> (sah x i) `elem` [qah x j | j <- [2 * i - 1, 2 * i]]

    it "years start at the beginning of a quarter" $ property $
      \ x i -> (yah x i) `elem` [qah x j | j <- [4 * i - 3, 4 * i - 2, 4 * i - 1, 4 * i]]

    it "years never start at the beginning of a season" $ property $
      \ x i -> (yah x i) `notElem` [sah x j | j <- [2 * i - 1, 2 * i]]

    it "nextPeakPeriod never returns a date smaller than the given date" $ property $
      \ x -> (fst . nextPeakPeriod $ x) >= x

    it "the beginning of a peak period is always before its end" $ property $
      \ x -> (fst . nextPeakPeriod $ x) < (snd . nextPeakPeriod $ x)

    it "nextOffpeakPeriod never returns a date smaller than the given date" $ property $
      \ x -> (fst . nextOffpeakPeriod $ x) >= x

    it "the beginning of an offpeak period is always before its end" $ property $
      \ x -> (fst . nextOffpeakPeriod $ x) < (snd . nextOffpeakPeriod $ x)

    it "a datetime is either in a peak or offpeak period" $ property $
      \ x -> x == (fst . nextPeakPeriod $ x) || x == (fst . nextOffpeakPeriod $ x)

    it "the end of a peak period is the beginning of the following offpeak period " $ property $
      \ x -> (snd . nextPeakPeriod $ x) == (fst . nextOffpeakPeriod . fst . nextPeakPeriod $ x)

    it "the end of an offpeak period is the beginning of the following peak period " $ property $
      \ x -> (snd . nextOffpeakPeriod $ x) == (fst . nextPeakPeriod . fst . nextOffpeakPeriod $ x)