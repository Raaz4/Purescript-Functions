module Main where

import Prelude (Unit, discard, ($), (+))
import Data.Tuple (Tuple(..), curry, fst, lookup, snd, swap, uncurry)
import Effect (Effect)
import Effect.Class.Console (logShow, log)

add :: Int -> Int -> Int
add a b = a + b

main :: Effect Unit
main = do
  log $ "fst :: forall a b. Tuple a b -> a"
  logShow $ fst $ Tuple 7 9
  log $ "snd :: forall a b. Tuple a b -> b"
  logShow $ snd $ Tuple 8 3
  log $ "curry :: forall a b c. (Tuple a b -> c) -> a -> b -> c"
  logShow $ curry fst 4 6
  log $ "uncurry :: forall a b c. (a -> b -> c) -> Tuple a b -> c"
  logShow $ uncurry add (Tuple 7 9)
  log $ "swapping :: forall a b. Tuple a b -> Tuple b a"
  logShow $ swap (Tuple 90 60)
  log $ "lookup :: forall a b f. Foldable f => Eq a => a -> f (Tuple a b) -> Maybe b"
  logShow $ lookup 55 [(Tuple 5 9), (Tuple 90 55)]
  
