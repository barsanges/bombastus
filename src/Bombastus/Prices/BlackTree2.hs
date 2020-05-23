{- |
   Module      : Bombastus.Prices.BlackTree2
   Copyright   : Copyright (C) 2020 barsanges
   License     : GNU GPL, version 3

A binomial tree for the Black (1976) model.
-}

module Bombastus.Prices.BlackTree2 (
  BlackTree2,
  mkTree
  ) where

import Bombastus.DateTime
import Bombastus.Numerics
import Bombastus.Prices.Currency ( Currency )
import Bombastus.Prices.Product ( Product, getCurrency )
import Bombastus.Prices.Provider
import Math.Combinatorics.Exact.Binomial ( choose )

data Parameters = Parameters { d :: Double, u :: Double, pu :: Double }

-- | A binomial tree for the Black (1976) model, with a step size of one day.
data BlackTree2 = BlackTree2 { parameters :: Maybe Parameters
                             , r :: Double
                             , t0 :: Date
                             , f0 :: Double
                             , prod :: Product
                             }

instance Provider BlackTree2 where
  getDF = getDF'
  getFX = getFX'
  getPrices = getPrices'

-- | Build a tree from an instance of the Black model.
mkTree :: DateTime   -- ^ Initial date
       -> Product    -- ^ Product whose value follows a Black model
       -> Double     -- ^ Value of the produt at the initial date
       -> Double     -- ^ Black volatility
       -> Double     -- ^ Interest rate
       -> BlackTree2 -- ^ Resulting tree
mkTree t p value sigma rate = BlackTree2 { parameters = params
                                         , r = rate
                                         , t0 = asDate t
                                         , f0 = value
                                         , prod = p
                                         }
  where
    params = if sigma > 0
             then Just $ Parameters { d = d', u = u', pu = pu' }
             else Nothing
    step = 1 / 365
    d' = exp (-sigma * sqrt step)
    u' = exp (sigma * sqrt step)
    pu' = (exp (rate * step) - d') / (u' - d')

nans :: Int -> Xd
nans n = fromList [nan | _ <- [0..n] ]

index :: BlackTree2 -> Date -> Int
index tree t = diffDateInDays t (t0 tree)

getDF' :: BlackTree2 -> Currency -> DateTime -> Date -> Xd
getDF' tree curr t maturity
  | (getCurrency . prod $ tree) == curr && now <= maturity = fromList [df | _ <- [0..n] ]
  | otherwise = nans n
  where
    now = asDate t
    n = index tree now
    dt = diffDateInYears now maturity
    df = exp (-(r tree) * dt)

getFX' :: BlackTree2 -> Currency -> Currency -> DateTime -> Date -> Xd
getFX' tree curr1 curr2 t maturity
  | sameCurrency && asDate t <= maturity = fromList [1 | _ <- [0..n] ]
  | otherwise = nans n
  where
    sameCurrency = (curr1 == curr2 && curr1 == (getCurrency . prod $ tree))
    n = index tree (asDate t)

getLevel :: BlackTree2 -> Int -> Xd
getLevel tree n
  | n < 0 = fromList []
  | otherwise = case parameters tree of
      Just p -> fromList [ (f0 tree) * ((d p) ** i') * ((u p) ** (n' - i'))
                           | i <- [0..n],
                             let i' = fromIntegral i,
                             let n' = fromIntegral n ]
      Nothing -> fromList [f0 tree]

getPrices' :: BlackTree2 -> Product -> DateTime -> Xd
getPrices' tree p t
  | prod tree == p = getLevel tree n
  | otherwise = nans n
  where
    n = index tree (asDate t)

conditionalExpectation :: BlackTree2 -> Int -> Xd -> Xd
conditionalExpectation bt n values
  | n > lengthXd values = nans n
  | otherwise = fromList [ sum [ (values ! (i + j)) * fromIntegral (choose k j)
                               | j <- [0..k] ]
                         | i <- [0..n] ]
  -- FIXME: there is a probably a better/faster way to write this?
  where
    k = n - lengthXd values

regress :: BlackTree2 -> DateTime -> Xd -> Xd
regress bt t values = conditionalExpectation bt n values
  where
    n = index bt (asDate t)