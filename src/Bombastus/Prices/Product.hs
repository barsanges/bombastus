{- |
   Module      : Bombastus.Prices.Product
   Copyright   : Copyright (C) 2020 barsanges
   License     : GNU GPL, version 3

Forward product on commodity markets.
-}

module Bombastus.Prices.Product (
  Profile(..),
  SeasonType(..),
  Relative(..),
  Absolute(..),
  Product(..),
  getCurrency,
  getProfile,
  getDeliveryStart,
  getDeliveryEnd
  ) where

import Bombastus.DateTime
import Bombastus.Prices.Currency ( Currency )
import Bombastus.Prices.Market ( Market(Market) )

-- | Delivery profile.
data Profile = Base
             | Peak
             | Offpeak
             | None -- ^ When profile is irrelevant
  deriving (Eq, Show)

-- | A season can be either winter or summer.
data SeasonType = Winter | Summer
  deriving (Eq, Show)

-- | Relative product: the delivery dates depend on the quotation date.
data Relative = DAH Int -- ^ Number of days from quotation date
              | WAH Int -- ^ Number of weeks (starting on mondays) from quotation date
              | MAH Int -- ^ Number of months from quotation date
              | QAH Int -- ^ Number of quarters from quotation date
              | SAH Int -- ^ Number of seasons from quotation date
              | YAH Int -- ^ Number of calendar years from quotation date
              | FreeR NominalDiffTime NominalDiffTime -- ^ Delivery start and end
  deriving (Eq, Show)

-- | Absolute product: the delivery dates do not depend on the quotation date.
data Absolute = Week Int Int -- ^ Year and week number (1..53)
              | Month Int Int -- ^ Year and month number (1..12)
              | Quarter Int Int -- ^ Year and quarter number (1..4)
              | Season Int SeasonType -- ^ Year and season type (winter or summer)
              | Year Int
              | FreeA DateTime DateTime -- ^ Delivery start and end
  deriving (Eq, Show)

-- | Forward product for a given delivery.
data Product = Product Market (Either Relative Absolute) Profile
  deriving (Eq, Show)

-- | Get the currency in which a product is quoted.
getCurrency :: Product -> Currency
getCurrency (Product (Market _ curr) _ _) = curr

-- | Get the profile of a product.
getProfile :: Product -> Profile
getProfile (Product _ _ prof) = prof

-- | Get the first date in the delivery period of a product.
getDeliveryStart :: Product -> DateTime -> DateTime
getDeliveryStart (Product _ (Left p) _) t = getDeliveryStartRelative p t
getDeliveryStart (Product _ (Right p) _) _ = getDeliveryStartAbsolute p

getDeliveryStartRelative :: Relative -> DateTime -> DateTime
getDeliveryStartRelative (DAH i) t = dah t i
getDeliveryStartRelative (WAH i) t = wah t i
getDeliveryStartRelative (MAH i) t = mah t i
getDeliveryStartRelative (QAH i) t = qah t i
getDeliveryStartRelative (SAH i) t = sah t i
getDeliveryStartRelative (YAH i) t = yah t i
getDeliveryStartRelative (FreeR i _) t = addUTCTime i t

getDeliveryStartAbsolute :: Absolute -> DateTime
getDeliveryStartAbsolute (Week y w) = weekToDateTime y w
getDeliveryStartAbsolute (Month y m) = normalizedDateTime y m 1
getDeliveryStartAbsolute (Quarter y q) = normalizedDateTime y ((q - 1) * 3 + 1) 1
getDeliveryStartAbsolute (Season y Summer) = normalizedDateTime y 4 1
getDeliveryStartAbsolute (Season y Winter) = normalizedDateTime y 10 1
getDeliveryStartAbsolute (Year y) = normalizedDateTime y 1 1
getDeliveryStartAbsolute (FreeA s _) = s

-- | Get the first date not in the delivery period of a product.
getDeliveryEnd :: Product -> DateTime -> DateTime
getDeliveryEnd (Product _ (Left p) _) t = getDeliveryEndRelative p t
getDeliveryEnd (Product _ (Right p) _) _ = getDeliveryEndAbsolute p

getDeliveryEndRelative :: Relative -> DateTime -> DateTime
getDeliveryEndRelative (DAH i) t = dah t (i + 1)
getDeliveryEndRelative (WAH i) t = wah t (i + 1)
getDeliveryEndRelative (MAH i) t = mah t (i + 1)
getDeliveryEndRelative (QAH i) t = qah t (i + 1)
getDeliveryEndRelative (SAH i) t = sah t (i + 1)
getDeliveryEndRelative (YAH i) t = yah t (i + 1)
getDeliveryEndRelative (FreeR _ j) t = addUTCTime j t

getDeliveryEndAbsolute :: Absolute -> DateTime
getDeliveryEndAbsolute p = case p of
  Week _ _ -> wah (getDeliveryStartAbsolute p) 1
  Month _ _ -> mah (getDeliveryStartAbsolute p) 1
  Quarter _ _ -> qah (getDeliveryStartAbsolute p) 1
  Season _ _ -> sah (getDeliveryStartAbsolute p) 1
  Year _ -> yah (getDeliveryStartAbsolute p) 1
  FreeA _ e -> e
