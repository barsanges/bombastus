{- |
   Module      : Bombastus.Prices.Market
   Copyright   : Copyright (C) 2020 barsanges
   License     : GNU GPL, version 3

Forward commodity market (e.g. NBP, TTF, German power).
-}

module Bombastus.Prices.Market (
  Market(..)
  ) where

import Bombastus.Prices.Currency ( Currency )

-- | Market defined by its name and its currency.
data Market = Market String Currency -- FIXME : use Text?
  deriving (Eq, Show)