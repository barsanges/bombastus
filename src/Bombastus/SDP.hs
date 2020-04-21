{- |
   Module      : Bombastus.SDP
   Copyright   : Copyright (C) 2020 barsanges
   License     : GNU GPL, version 3

Functions to solve stochastic dynamic programming.
-}

module Bombastus.SDP (
  bkwd
  ) where

-- | Map on a list, using both the current value and the previous result.
inductiveMap :: (a -> b -> b) -> b -> [a] -> [b]
inductiveMap _ _ [] = []
inductiveMap f y (x:xs) = let y' = f x y
                          in (y':inductiveMap f y' xs)

-- | Backward phase of the stochastic dynamic programming algorithm.
bkwd :: (Functor f1, Functor f2, Foldable f2, Ord x)
     => (d -> f1 s)             -- ^ Return admissible states
     -> (d -> s -> f2 n)        -- ^ Return admissibles nominations
     -> (d -> s -> n -> s)      -- ^ Build the next state
     -> (d -> s -> n -> x)      -- ^ Return the value of a nomination
     -> (d -> p)                -- ^ Return particles
     -> (x -> x -> x)           -- ^ Add continuation values
     -> (p -> x -> y)           -- ^ Regress continuation values
     -> (f1 y -> (s -> p -> x)) -- ^ Interpolate continuation values
     -> (s -> p -> x)           -- ^ Final value
     -> [d]                     -- ^ Steps (from last to first)
     -> [s -> p -> x]           -- ^ Continuation values estimators
bkwd states nominations next eval particles add regress interpolate end steps = inductiveMap go end steps
  where
    go step condexp = interpolate $ fmap processState (states step)
      where
        p = particles step
        total state n = add (eval step state n) (condexp (next step state n) p)
        processState state = regress p $ maximum $ fmap (total state) $ nominations step state