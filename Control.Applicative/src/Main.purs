module Main where

import Control.Applicative (apply, liftA1, unless, when)
import Data.Int (quot)
import Data.Maybe (Maybe (..))
import Effect (Effect)
import Effect.Class.Console (logShow, log)
import Prelude
import Math (ceil)

-- Instances
-- Applicative (Function r)
-- Applicative Array

ad :: Int -> Int
ad a = a + 5

arrayint :: Array Int
arrayint = [1,2,5,4,6,7,4,9,8,5,3,8]

arraynum :: Array Number
arraynum =  [1.2,43.43,3.42,3.42,42.2,23.23]

sub :: Int -> Int
sub a = 5 - a

main :: Effect Unit
main = do
  log $ "apply : forall f a b. Apply f => f (a -> b) -> f a -> f b"
  logShow $ apply [ad, sub] arrayint
  log $ "liftA1 : forall f a b. Applicative f => (a -> b) -> f a -> f b"
  logShow $ liftA1 ceil arraynum
  log $ "unless : forall m. Applicative m => Boolean -> m Unit -> m Unit"
  logShow $ unless true $ Just unit
  log $ "when : forall m. Applicative m => Boolean -> m Unit -> m Unit"
  logShow $ when false $ Just unit

  --  Operators
  log $ "------------ Operators -------------"
  logShow $ quot <$> arrayint <*> arrayint
  logShow $ [ad, sub] <*> arrayint -- Operator alias for Control.Apply.apply (left-associative / precedence 4)
  logShow $ arrayint <* [ad, sub] -- Operator alias for Control.Apply.applyFirst (left-associative / precedence 4)
  logShow $ [ad, sub] *> arrayint -- Operator alias for Control.Apply.applySecond (left-associative / precedence 4)
  logShow $ ad <$> arrayint -- Operator alias for Data.Functor.map (left-associative / precedence 4)
  logShow $ arrayint <#> ad -- Operator alias for Data.Functor.mapFlipped (left-associative / precedence 1)
  logShow $ 3 <$ arrayint -- Operator alias for Data.Functor.voidRight (left-associative / precedence 4)
  logShow $ arrayint $> 6 -- Operator alias for Data.Functor.voidLeft (left-associative / precedence 4)
