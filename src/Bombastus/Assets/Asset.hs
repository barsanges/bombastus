{- |
   Module      : Bombastus.Assets.Asset
   Copyright   : Copyright (C) 2020 barsanges
   License     : GNU GPL, version 3

Asset that can be optimized thanks to the stochastic dynamic programming.
-}

module Bombastus.Assets.Asset (
  Asset(..)
  ) where

import Data.Set ( Set )
import Data.Time ( UTCTime ) -- TODO : take time zones into account
import Bombastus.Numerics

data Asset f1 s f2 n = Asset { nominationDates :: Set UTCTime
                             , getStates :: UTCTime -> f1 s
                             , getAdmissibleNominations :: UTCTime -> s -> f2 n
                             , nextState :: UTCTime -> s -> n -> s
                             , getValue :: UTCTime -> s -> n -> Xd
                             }