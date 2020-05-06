{- |
   Module      : Bombastus.Numerics
   Copyright   : Copyright (C) 2020 barsanges
   License     : GNU GPL, version 3

Vectors of double and functions to calculate with them.
-}

module Bombastus.Numerics (
  Xd,
  fromList,
  lengthXd,
  zerosLike,
  onesLike,
  (+.),
  (.+),
  (.+.),
  (-.),
  (.-),
  (.-.),
  (*.),
  (.*),
  (.*.),
  (/.),
  (./),
  (./.),
  almostEqualXd,
  almostEqualXd',
  maxXd,
  maxXd',
  minXd,
  minXd',
  positiveXd,
  clipXd,
  absXd,
  expXd
  ) where

import Data.Vector ( fromList )
import qualified Data.Vector as V -- FIXME: should we use unboxed vectors?

infixl 7 .*, *., .*., ./, /., ./.
infixl 6 .+, +., .+., .-, -., .-.

-- FIXME: add (.>.), (.<.) and/or something like numpy.where?

-- | One dimensional vector.
type Xd = V.Vector Double

-- | Yield the length of the vector.
lengthXd :: Xd -> Int
lengthXd = V.length

-- | Return a vector of 0s. It has the same size as the input.
zerosLike :: Xd -> Xd
zerosLike x = V.replicate (V.length x) 0

-- | Return a vector of 1s. It has the same size as the input.
onesLike :: Xd -> Xd
onesLike x = V.replicate (V.length x) 1

-- | Add a double to each element of a vector.
(+.) :: Double -> Xd -> Xd
a +. x = V.map (a+) x

-- | Add a double to each element of a vector.
(.+) :: Xd -> Double -> Xd
x .+ a = a +. x

-- | Add two vectors, element-wise.
(.+.) :: Xd -> Xd -> Xd
(.+.) = V.zipWith (+)

-- | Substract each element of a vector to a double (return a vector).
(-.) :: Double -> Xd -> Xd
a -. x = V.map (a-) x

-- | Substract a double to each element of a vector.
(.-) :: Xd -> Double -> Xd
x .- a = V.map ((+) (-a)) x

-- | Substract two vectors, element-wise.
(.-.) :: Xd -> Xd -> Xd
(.-.) = V.zipWith (-)

-- | Multiply each element of a vector by a double.
(*.) :: Double -> Xd -> Xd
a *. x = V.map (a*) x

-- | Multiply each element of a vector by a double.
(.*) :: Xd -> Double -> Xd
x .* a = a *. x

-- | Multiply two vectors, element-wise.
(.*.) :: Xd -> Xd -> Xd
(.*.) = V.zipWith (*)

-- | Divide a double by each element of a vector (return a vector).
(/.) :: Double -> Xd -> Xd
a /. x = V.map (a/) x

-- | Divide each element of a vector by a double.
(./) :: Xd -> Double -> Xd
x ./ a = V.map (/a) x

-- | Divide two vectors, element-wise.
(./.) :: Xd -> Xd -> Xd
(./.) = V.zipWith (/)

-- | Test if two vectors are equal element-wise (with a precision of 1e-9).
almostEqualXd :: Xd -> Xd -> Bool
almostEqualXd = almostEqualXd' 1e-9

-- | Test if two vectors are "equal" (for the given precision) element-wise.
almostEqualXd' :: Double -> Xd -> Xd -> Bool
almostEqualXd' eps x y = if lengthXd x == lengthXd y
                         then if V.null x
                              then True
                              else (V.maximum . absXd $ (x .-. y)) < eps
                         else False

-- | Element-wise maximum of two vectors.
maxXd :: Xd -> Xd -> Xd
maxXd = V.zipWith max

-- | Element-wise maximum of a vector and a double (which is treated as a
-- constant vector).
maxXd' :: Double -> Xd -> Xd
maxXd' a = V.map (max a)

-- | Element-wise minimum of two vectors.
minXd :: Xd -> Xd -> Xd
minXd = V.zipWith min

-- | Element-wise minimum of a vector and a double (which is treated as a
-- constant vector).
minXd' :: Double -> Xd -> Xd
minXd' a = V.map (min a)

-- | Positive part of a vector (i.e.: negative values are clipped to 0).
positiveXd :: Xd -> Xd
positiveXd = maxXd' 0

-- | Clip a vector between two values.
clipXd :: Double -> Double -> Xd -> Xd
clipXd a b x = if a <= b
               then maxXd' a $ minXd' b x
               else clipXd b a x

-- | Element-wise absolute value of a vector.
absXd :: Xd -> Xd
absXd = V.map abs

-- | Element-wise exponential of a vector.
expXd :: Xd -> Xd
expXd = V.map exp