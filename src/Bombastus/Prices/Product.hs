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
  getCurrency
  ) where

import Bombastus.DateTime ( DateTime, NominalDiffTime )
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