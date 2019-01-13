module Main where

import Prelude
import Effect (Effect)
import Effect.Class.Console (logShow)
import Data.Unfoldable (fromMaybe)
import Data.Unfoldable
import Data.Maybe
import Data.Traversable
import Data.Tuple

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
  -- logShow $ replicate 4 "this"
  -- logShow $ replicateA1 3 [5, 7, 9]
  -- logShow $ none1
  -- logShow $ fromMaybe1 $ Just 8
