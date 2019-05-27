module Main where

import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Prelude (class Eq, class EuclideanRing, Unit, discard, gcd, lcm, ($), (/))

foreign import intDegree :: Int -> Int
foreign import intDiv :: Int -> Int -> Int
foreign import intMod :: Int -> Int -> Int
foreign import numDiv :: Number -> Number -> Number

main :: Effect Unit
main = do
  log $ "gcd :: forall a. Eq a => EuclideanRing a => a -> a -> a"
  logShow $ gcd 101 202
  logShow $ intDegree 4
  logShow $ intDiv 20 9
  logShow $ 20 `intDiv` 9
  logShow $ intMod 9 2
  logShow $ numDiv 4.5 6.5
  logShow $ 9 `intMod` 3
  logShow $ 4.3 `numDiv` 9.3
  log $ "lcm :: forall a. Eq a => EuclideanRing a => a -> a -> a"
  logShow $ lcm 6 7
  logShow $ 5 / 2 -- Operator alias for Data.EuclideanRing.div (left-associative / precedence 7)
