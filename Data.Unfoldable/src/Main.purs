module Main where

import Prelude (Unit, discard, ($))
import Effect (Effect)
import Effect.Class.Console (logShow,log)
import Data.Unfoldable (fromMaybe, none, replicate, replicateA)
import Data.Maybe (Maybe(..))
import Data.List (List)

main :: Effect Unit
main = do
  log $ "replicate :: forall f a. Unfoldable f => Int -> a -> f a"
  logShow $ replicate 4 "this" :: Array _
  log $ "replicateA :: forall m f a. Applicative m => Unfoldable f => Traversable f => Int -> m a -> m (f a)"
  logShow $ replicateA 3 [5, 7, 9] :: Array (Array _)
  log $ "none :: forall f a. Unfoldable f => f a"
  logShow $ none :: List Int
  log $ "fromMaybe :: forall f a. Unfoldable f => Maybe a -> f a"
  logShow $ fromMaybe (Just 8) :: Array _
