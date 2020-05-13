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
  addUTCTime,
  diffTimeInHours,
  asDate,
  normalizedDateTime,
  weekToDateTime,
  dah,
  wah,
  mah,
  qah,
  sah,
  yah,
  nextPeakPeriod,
  nextOffpeakPeriod
  ) where

import Data.Time
import Data.Time.Calendar.WeekDate ( fromWeekDate, toWeekDate )

-- | A date (like 25/12/2020) without time within the day.
type Date = Day

-- | A date (like 25/12/2020) and a time (like 23:00:00).
type DateTime = UTCTime -- TODO : take daylight saving time into account.

-- | Return the time span between two datetimes, in hours (1 = 1 hour,
-- 0.5 = 30 min, etc).
diffTimeInHours :: DateTime -> DateTime -> Double
diffTimeInHours x y = (realToFrac $ diffUTCTime x y) / 3600-- FIXME: handle daylight saving time.

-- | Return the date component of a date time.
asDate :: DateTime -> Date
asDate = utctDay

-- | Return a date time corresponding to the given year, month and day.
normalizedDateTime :: Int -> Int -> Int -> DateTime
normalizedDateTime year month day = UTCTime { utctDay = date, utctDayTime = 0 }
  where
    date = fromGregorian (toInteger year) month day

-- | Return a date time corresponding to the given year and week.
weekToDateTime :: Int -> Int -> DateTime
weekToDateTime year week = UTCTime { utctDay = date, utctDayTime = 0 }
  where
    date = fromWeekDate (toInteger year) week 1

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
    month' = 3 * ((month - 1) `quot` 3) + 1

-- | Return the first day of the season of the given day.
seasonStart :: Date -> Date
seasonStart d = fromGregorian year' month' 1
  where
    (year, month, _) = toGregorian d
    (year', month') = if (month < 4)
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

-- | Return the day of the week (1 for Monday, etc). A similar function exists
-- in time 1.10, and should be used as soon as we use the right version of the
-- library.
dayOfWeek :: Date -> Int
dayOfWeek (ModifiedJulianDay d) = (mod (fromInteger d + 2) 7) + 1

-- | Return the first peak datetime greater or equal than the given time.
nextPeak :: DateTime -> DateTime
nextPeak t
  | dayOfWeek today <= 5 && hours < 8 = UTCTime { utctDay = today, utctDayTime = eightAM}
  | dayOfWeek today <= 5 && hours >= 8 && hours < 20 = t
  | dayOfWeek today <= 4 && hours >= 20 = UTCTime { utctDay = tomorrow, utctDayTime = eightAM}
  | otherwise = UTCTime {utctDay = monday, utctDayTime = eightAM }
  where
    hours = todHour . timeToTimeOfDay . utctDayTime $ t
    today = utctDay t
    tomorrow = utctDay $ dah t 1
    monday = utctDay $ wah t 1
    eightAM = timeOfDayToTime $ TimeOfDay 8 0 0

-- | Return the first offpeak datetime greater or equal than the given time.
nextOffpeak :: DateTime -> DateTime
nextOffpeak t = if (dayOfWeek . utctDay $ t) > 5 || hours < 8 || hours >= 20
                then t
                else UTCTime { utctDay = utctDay t, utctDayTime = eightPM }
  where
    hours = todHour . timeToTimeOfDay . utctDayTime $ t
    eightPM = timeOfDayToTime $ TimeOfDay 20 0 0

-- | Return the bounds of the next peak period (lower bound: first datetime in
-- the peak period; upper bound: first date greater than the previous not in the
-- peak period).
nextPeakPeriod :: DateTime -> (DateTime, DateTime)
nextPeakPeriod t = (low, up)
  where
    low = nextPeak t
    up = nextOffpeak (max low t)

-- | Return the bounds of the next offpeak period (lower bound: first datetime
-- in the offpeak period; upper bound: first date greater than the previous not
-- in the offpeak period).
nextOffpeakPeriod :: DateTime -> (DateTime, DateTime)
nextOffpeakPeriod t = (low, up)
  where
    low = nextOffpeak t
    up = nextPeak (max low t)