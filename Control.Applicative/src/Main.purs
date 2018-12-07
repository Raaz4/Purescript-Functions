module Main where

import Prelude
import Effect (Effect)
import Effect.Class.Console (logShow)
import Control.Applicative
import Math
import Data.Maybe

quot :: Int -> Int -> Int
quot a b = a / b

ad a = a + 5
sub a = 5 - a

arraynum =  [1.2,43.43,3.42,3.42,42.2,23.23]

arrayint = [1,2,5,4,6,7,4,9,8,5,3,8]

apply1 :: forall f a b. Apply f => f (a -> b) -> f a -> f b
apply1 a b = apply a b

liftA11 :: forall f a b. Applicative f => (a -> b) -> f a -> f b
liftA11 f array = liftA1 f array

unless1 :: forall m. Applicative m => Boolean -> m Unit -> m Unit
unless1 b u = unless b u

when1 :: forall m. Applicative m => Boolean -> m Unit -> m Unit
when1 b u = when b u

main :: Effect Unit
main = do
  logShow (apply1 [ad, sub] arrayint)
  logShow (liftA11 ceil arraynum)
  logShow (unless1 true (Just unit))
  logShow (when1 false (Just unit))

  --  Operators
  
  logShow (quot <$> arrayint <*> arrayint)
  logShow ([ad, sub] <*> arrayint)
  logShow (arrayint <* [ad, sub])
  logShow ([ad, sub] *> arrayint)
  logShow (ad <$> arrayint)
  logShow (3 <$ arrayint)
  logShow (arrayint $> 6)
  logShow (arrayint <#> ad)
