module Main where

import Prelude (class Applicative, Unit, discard, ($))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Data.Unfoldable (class Unfoldable, fromMaybe, none, replicate, replicateA)
import Data.Maybe (Maybe(..))
import Data.List (List)
import Data.Traversable (class Traversable)

replicate1 :: forall f a. Unfoldable f => Int -> a -> f a
replicate1 a b = replicate a b

replicateA1 :: forall m f a. Applicative m => Unfoldable f => Traversable f => Int -> m a -> m (f a)
replicateA1 i m = replicateA i m

none1 :: forall f a. Unfoldable f => f a
none1 = none

fromMaybe1 :: forall f a. Unfoldable f => Maybe a -> f a
fromMaybe1 m = fromMaybe m

main :: Effect Unit
main = do
  logShow $ replicate 4 "this" :: Array _
  logShow $ replicateA1 3 [5, 7, 9] :: Array (Array _)
  logShow $ none1 :: List Int
  logShow $ fromMaybe1 (Just 8) :: Array _
