module Main where

import Prelude (class Functor, Unit, discard, flap, void, ($), (+), (-))
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Data.Functor (mapFlipped, voidLeft, voidRight, (<$>), (<#>), (<$), ($>), (<@>))
-- Instances
-- Functor (Function r)
-- Functor Array

add :: Int -> Int
add a = a + 6

array :: Array Int
array = [1,2,5,4,68,54,3,2,2,3]

main :: Effect Unit
main = do
  log $ "mapFlipped :: forall f a b. Functor f => f a -> (a -> b) -> f b"
  logShow $ mapFlipped array add
  log $ "void :: forall f a. Functor f => f a -> f Unit"
  logShow $ void array
  log $ "voidRight :: forall f a b. Functor f => a -> f b -> f a"
  logShow $ voidRight 3 array
  log $ "voidLeft :: forall f a b. Functor f => f a -> b -> f b"
  logShow $ voidLeft array 9
  log $ "flap :: forall f a b. Functor f => f (a -> b) -> a -> f b"
  logShow $ flap (-) 5 9
  log $ "------------ Operators -------------"
  logShow $ add <$> array -- Operator alias for Data.Functor.map (left-associative / precedence 4)
  logShow $ array <#> add -- Operator alias for Data.Functor.mapFlipped (left-associative / precedence 1)
  logShow $ 3 <$ array -- Operator alias for Data.Functor.voidRight (left-associative / precedence 4)
  logShow $ array $> 5 -- Operator alias for Data.Functor.voidLeft (left-associative / precedence 4)
  logShow $ [add] <@> 5 -- Operator alias for Data.Functor.flap (left-associative / precedence 4)