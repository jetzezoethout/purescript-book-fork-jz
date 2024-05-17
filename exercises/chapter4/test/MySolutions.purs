module Test.MySolutions where

import Prelude

import ChapterExamples (Amp(..), Volt(..))
import Data.Maybe (Maybe(..))
import Data.Number (pi)
import Data.Person (Person)
import Data.Picture (Shape(..), origin)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

binomial :: Int -> Int -> Int
binomial n k
  | n < k = 0
  | otherwise = factorial n / (factorial k * factorial (n - k))

pascal :: Int -> Int -> Int
pascal n k
  | n < k = 0
  | k == 0 = 1
  | k == n = 1
  | otherwise = pascal (n - 1) k + pascal (n - 1) (k - 1)

sameCity :: Person -> Person -> Boolean -- This is very nice!
sameCity { address: { city: city1 } } { address: { city: city2 } } = city1 == city2

fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [ x ] = x
fromSingleton default _ = default

circleAtOrigin :: Shape
circleAtOrigin = Circle origin 10.0

doubleScaleAndCenter :: Shape -> Shape
doubleScaleAndCenter (Circle _ radius) = Circle origin (2.0 * radius)
doubleScaleAndCenter (Rectangle _ width height) = Rectangle origin (2.0 * width) (2.0 * height)
doubleScaleAndCenter (Line start end) = Line (start - end) (end - start)
doubleScaleAndCenter (Text _ text) = Text origin text
doubleScaleAndCenter (Clipped picture _ width height) = Clipped picture origin width height

shapeText :: Shape -> Maybe String
shapeText (Text _ text) = Just text
shapeText _ = Nothing

newtype Watt = Watt Number

calculateWattage :: Amp -> Volt -> Watt
calculateWattage (Amp a) (Volt v) = Watt $ a * v

area :: Shape -> Number
area (Circle _ radius) = pi * radius * radius
area (Rectangle _ width height) = width * height
area _ = 0.0