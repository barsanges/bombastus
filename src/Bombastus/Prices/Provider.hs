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

import Data.Vector ( Vector )
import qualified Data.Vector as V
import Bombastus.DateTime
import Bombastus.Prices.Currency ( Currency )
import Bombastus.Prices.Product ( Product, getCurrency, getDeliveryStart )

-- | Structure handling market prices.
class Provider p where
  getDF :: p -> Currency -> DateTime -> Date -> Vector Double
  getFX :: p -> Currency -> Currency -> DateTime -> Date -> Vector Double
  getPrices :: p -> Product -> DateTime -> Vector Double

-- | Get the price of a product in a given currency.
getPricesInCurrency :: Provider p => p -> Product -> Currency -> DateTime -> Vector Double
getPricesInCurrency provider prod curr t = V.zipWith (*) fx base -- TODO: move "V.zipWith (*)" in a module "Numerics".
  where
    curr' = getCurrency prod
    maturity = asDate $ getDeliveryStart prod t
    -- FIXME: this is an approximation. We should take the average FX on the
    -- delivery period.
    fx = getFX provider curr' curr t maturity
    base = getPrices provider prod t