module Test.MySolutions where

import Prelude

import Control.Alternative (guard)
import Data.Array (cons, filter, foldl, foldr, head, length, sortBy, uncons, unsnoc, (..))
import Data.Function (on)
import Data.Maybe (Maybe(..))
import Data.Path (Path, filename, isDirectory, ls, size)
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import Test.Examples (allFiles, factors)

-- Note to reader: Add your solutions to this file

isEven :: Int -> Boolean
isEven 0 = true
isEven 1 = false
isEven x
  | x < 0 = isEven $ x + 2
  | otherwise = isEven $ x - 2

countEven :: Array Int -> Int
countEven arr = case uncons arr of
  Nothing -> 0
  Just { head, tail } -> (if head `mod` 2 == 0 then 1 else 0) + countEven tail

squared ∷ Array Number -> Array Number
squared = map $ \x -> x * x

keepNonNegative ∷ Array Number → Array Number
keepNonNegative = filter $ \x -> x >= 0.0

infix 4 filter as <$?>

keepNonNegativeRewrite ∷ Array Number → Array Number
keepNonNegativeRewrite xs = (\x -> x >= 0.0) <$?> xs

isPrime ∷ Int → Boolean
isPrime n = n > 1 && length (factors n) == 1

cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct xs ys = do
  x <- xs
  y <- ys
  pure $ [ x, y ]

triples :: Int -> Array (Array Int)
triples n = do
  a <- 1 .. n
  b <- a .. n
  c <- b .. n
  guard $ a * a + b * b == c * c
  pure [ a, b, c ]

primeFactors :: Int -> Array Int
primeFactors 1 = []
primeFactors n =
  let
    p = smallestFactor n
  in
    cons p $ primeFactors $ n `div` p
  where
  smallestFactor = smallestFactorFrom 2
  smallestFactorFrom start m =
    if m `mod` start == 0 then start
    else smallestFactorFrom (start + 1) m

allTrue :: Array Boolean -> Boolean
allTrue = foldr (&&) true

fibTailRec :: Int -> Int
fibTailRec = snd <<< fibPairs
  where
  fibPairs 0 = 1 /\ 0
  fibPairs n =
    let
      x /\ y = fibPairs $ n - 1
    in
      y /\ (x + y)

reverse :: forall a. Array a -> Array a
reverse = foldl (flip cons) []

onlyFiles :: Path -> Array Path
onlyFiles = filter (not <<< isDirectory) <<< allFiles

whereIs ∷ Path -> String -> Maybe Path
whereIs path toFind = head $ do
  subPath <- allFiles path
  child <- ls subPath
  guard $ filename subPath <> toFind == filename child
  pure subPath

largestSmallest :: Path -> Array Path
largestSmallest = headAndLast <<< sortBy (compare `on` size) <<< onlyFiles
  where
  headAndLast :: forall a. Array a -> Array a
  headAndLast xs = case uncons xs of
    Nothing -> []
    Just { head, tail } -> case unsnoc tail of
      Nothing -> [ head ]
      Just { last } -> [ head, last ]
