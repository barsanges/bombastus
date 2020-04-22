{- |
   Module      : Bombastus.Assets.Asset
   Copyright   : Copyright (C) 2020 barsanges
   License     : GNU GPL, version 3

Asset that can be optimized thanks to the stochastic dynamic programming.
-}

module Bombastus.Assets.Asset (
  Asset(..)
  ) where

import Data.Time ( UTCTime ) -- TODO : take time zones into account
import Data.Vector ( Vector )

data Asset f1 s f2 n = Asset { getStates :: UTCTime -> f1 s
                             , getNominations :: UTCTime -> s -> f2 n
                             , nextState :: UTCTime -> s -> n -> s
                             , getValue :: UTCTime -> s -> n -> Vector Double
                             }