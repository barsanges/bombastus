{- |
   Module      : Bombastus.Prices.Currency
   Copyright   : Copyright (C) 2020 barsanges
   License     : GNU GPL, version 3

Currency like USD, EUR, etc.
-}

module Bombastus.Prices.Currency (
  Currency(..)
  ) where

-- | ID of a currency.
newtype Currency = Currency String -- FIXME : use Text?
  deriving (Eq, Show)