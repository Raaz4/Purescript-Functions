module Main where

import Prelude (class Eq, Unit, discard, ($), (+))
import Data.Tuple (Tuple(..), curry, fst, lookup, snd, swap, uncurry)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Data.Maybe (Maybe)
import Data.Foldable (class Foldable)

add :: Int -> Int -> Int
add a b = a + b

first :: forall a b. Tuple a b -> a
first a = fst a

second :: forall a b. Tuple a b -> b
second a = snd a

curry1 :: forall a b c. (Tuple a b -> c) -> a -> b -> c
curry1 a b c = curry a b c

uncurry1 :: forall a b c. (a -> b -> c) -> Tuple a b -> c
uncurry1 a b = uncurry a b

swapping :: forall a b. Tuple a b -> Tuple b a
swapping a = swap a

lookup1 :: forall a b f. Foldable f => Eq a => a -> f (Tuple a b) -> Maybe b
lookup1 a array = lookup a array

main :: Effect Unit
main = do
  logShow $ first $ Tuple 7 9
  logShow $ second $ Tuple 8 3
  logShow $ curry1 fst 4 6
  logShow $ uncurry1 add (Tuple 7 9)
  logShow $ swapping (Tuple 90 60)
  logShow $ lookup1 55 [(Tuple 5 9), (Tuple 90 55)]
  
