{- |
   Module      : Bombastus.Prices.Provider
   Copyright   : Copyright (C) 2020 barsanges
   License     : GNU GPL, version 3

Typeclass and functions used to handle (simulated) market prices in pricing
algorithms.
-}

module Bombastus.Prices.Provider (
  Provider(..),
  getPricesInCurrency
  ) where

import Bombastus.DateTime
import Bombastus.Numerics
import Bombastus.Prices.Currency ( Currency )
import Bombastus.Prices.Product ( Product, getCurrency, getDeliveryStart )

-- | Structure handling market prices.
class Provider p where
  getDF :: p -> Currency -> DateTime -> Date -> Xd
  getFX :: p -> Currency -> Currency -> DateTime -> Date -> Xd
  getPrices :: p -> Product -> DateTime -> Xd

-- | Get the price of a product in a given currency.
getPricesInCurrency :: Provider p => p -> Product -> Currency -> DateTime -> Xd
getPricesInCurrency provider prod curr t = fx .*. base
  where
    curr' = getCurrency prod
    maturity = asDate $ getDeliveryStart prod t
    -- FIXME: this is an approximation. We should take the average FX on the
    -- delivery period.
    fx = getFX provider curr' curr t maturity
    base = getPrices provider prod t