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
import Bombastus.DateTime
import Bombastus.Numerics

data Asset f1 s f2 n = Asset { nominationDates :: Set DateTime
                             , getStates :: DateTime -> f1 s
                             , getAdmissibleNominations :: DateTime -> s -> f2 n
                             , nextState :: DateTime -> s -> n -> s
                             , getValue :: DateTime -> s -> n -> Xd
                             }