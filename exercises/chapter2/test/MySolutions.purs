module Test.MySolutions where

import Prelude

import Data.Int (rem)
import Data.Number (pi, pow, sqrt)

diagonal ∷ Number → Number → Number
diagonal w h = sqrt (w * w + h * h)

circleArea ∷ Number → Number
circleArea r = pi * pow r 2.0

leftoverCents :: Int -> Int
leftoverCents cents = cents `rem` 100
-- But of course, mod is always better :)
