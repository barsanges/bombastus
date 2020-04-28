{- |
   Module      : Bombastus.DateTime
   Copyright   : Copyright (C) 2020 barsanges
   License     : GNU GPL, version 3

Dates, time, and manipulation of both.
-}

module Bombastus.DateTime (
  Date,
  DateTime,
  NominalDiffTime,
  dah,
  wah,
  mah,
  qah,
  sah,
  yah
  ) where

import Data.Time
import Data.Time.Calendar.WeekDate ( fromWeekDate, toWeekDate )

-- | A date (like 25/12/2020) without time within the day.
type Date = Day

-- | A date (like 25/12/2020) and a time (like 23:00:00).
type DateTime = UTCTime -- TODO : take daylight saving time into account.

-- | Return the first day of the week of the given day.
weekStart :: Date -> Date
weekStart d = fromWeekDate year week 1
  where
    (year, week, _) = toWeekDate d

-- | Return the first day of the month of the given day.
monthStart :: Date -> Date
monthStart d = fromGregorian year month 1
  where
    (year, month, _) = toGregorian d

-- | Return the first day of the quarter of the given day.
quarterStart :: Date -> Date
quarterStart d = fromGregorian year month' 1
  where
    (year, month, _) = toGregorian d
    month' = month `quot` 4

-- | Return the first day of the season of the given day.
seasonStart :: Date -> Date
seasonStart d = fromGregorian year' month' 1
  where
    (year, month, _) = toGregorian d
    (year', month') = if (month < 3)
                      then (year - 1, 10)
                      else if (month < 10)
                           then (year, 4)
                           else (year, 10)

-- | Return the first day of the year of the given day.
yearStart :: Date -> Date
yearStart d = fromGregorian year 1 1
  where
    (year, _, _) = toGregorian d

-- | Return the first date of the n-th day ahead.
dah :: DateTime -> Int -> DateTime
dah t n = UTCTime { utctDay = date, utctDayTime = 0 }
  where
    date = addDays (toInteger n) $ utctDay t

-- | Return the first date of the n-th week ahead.
wah :: DateTime -> Int -> DateTime
wah t n = UTCTime { utctDay = date, utctDayTime = 0 }
  where
    date = addDays (7 * toInteger n) $ weekStart $ utctDay t

-- | Return the first date of the n-th month ahead.
mah :: DateTime -> Int -> DateTime
mah t n = UTCTime { utctDay = date, utctDayTime = 0 }
  where
    date = addGregorianMonthsClip (toInteger n) $ monthStart $ utctDay t

-- | Return the first date of the n-th quarter ahead.
qah :: DateTime -> Int -> DateTime
qah t n = UTCTime { utctDay = date, utctDayTime = 0 }
  where
    date = addGregorianMonthsClip (3 * toInteger n) $ quarterStart $ utctDay t

-- | Return the first date of the n-th season ahead.
sah :: DateTime -> Int -> DateTime
sah t n = UTCTime { utctDay = date, utctDayTime = 0 }
  where
    date = addGregorianMonthsClip (6 * toInteger n) $ seasonStart $ utctDay t

-- | Return the first date of the n-th year ahead.
yah :: DateTime -> Int -> DateTime
yah t n = UTCTime { utctDay = date, utctDayTime = 0 }
  where
    date = addGregorianYearsClip (toInteger n) $ yearStart $ utctDay t