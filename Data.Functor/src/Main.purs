module Main where

import Prelude (class Functor, Unit, discard, flap, void, ($), (+), (-))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Data.Functor (mapFlipped, voidLeft, voidRight)
-- Instances
-- Functor (Function r)
-- Functor Array

add :: Int -> Int
add a = a + 6

array :: Array Int
array = [1,2,5,4,68,54,3,2,2,3]

mapFlipped1 :: forall f a b. Functor f => f a -> (a -> b) -> f b
mapFlipped1 a b = mapFlipped a b

void1 :: forall f a. Functor f => f a -> f Unit
void1 a = void a

voidRight1 :: forall f a b. Functor f => a -> f b -> f a
voidRight1 a b = voidRight a b

voidLeft1 :: forall f a b. Functor f => f a -> b -> f b
voidLeft1  a b = voidLeft a b

flap1 :: forall f a b. Functor f => f (a -> b) -> a -> f b
flap1 a b = flap a b

main :: Effect Unit
main = do
  logShow $ mapFlipped1 array add
  logShow $ void1 array
  logShow $ voidRight1 3 array
  logShow $ voidLeft1 array 9
  logShow $ flap1 (-) 5 9