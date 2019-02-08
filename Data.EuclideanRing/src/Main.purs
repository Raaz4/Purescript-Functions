module Main where

import Prelude (class Eq, class EuclideanRing, Unit, discard, gcd, lcm, ($), (/))
import Effect (Effect)
import Effect.Class.Console (logShow)

foreign import intDegree :: Int -> Int
foreign import intDiv :: Int -> Int -> Int
foreign import intMod :: Int -> Int -> Int
foreign import numDiv :: Number -> Number -> Number

gcd1 :: forall a. Eq a => EuclideanRing a => a -> a -> a
gcd1 a b = gcd a b

lcm1 :: forall a. Eq a => EuclideanRing a => a -> a -> a
lcm1 a b = lcm a b

main :: Effect Unit
main = do
  logShow $ gcd1 101 202
  logShow $ intDegree 4
  logShow $ intDiv 20 9
  logShow $ 20 `intDiv` 9
  logShow $ intMod 9 2
  logShow $ numDiv 4.5 6.5
  logShow $ 9 `intMod` 3
  logShow $ 4.3 `numDiv` 9.3
  logShow $ lcm1 6 7
  logShow $ 5 / 2 -- Operator alias for Data.EuclideanRing.div (left-associative / precedence 7)
