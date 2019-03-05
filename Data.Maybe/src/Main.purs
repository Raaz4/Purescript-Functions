module Main where

import Prelude (Unit, discard, ($), (+))
import Effect (Effect)
import Effect.Class.Console (logShow, log)
import Data.Maybe (Maybe(..), fromJust, fromMaybe, fromMaybe', isJust, isNothing, maybe, maybe', optional)
import Partial.Unsafe (unsafePartial)

-- Instances
-- Functor Maybe
-- Apply Maybe
-- Applicative Maybe
-- Alt Maybe
-- Plus Maybe
-- Alternative Maybe
-- Bind Maybe
-- Monad Maybe
-- MonadZero Maybe
-- Extend Maybe
-- Invariant Maybe
-- (Semigroup a) => Semigroup (Maybe a)
-- (Semigroup a) => Monoid (Maybe a)
-- (Eq a) => Eq (Maybe a)
-- Eq1 Maybe
-- (Ord a) => Ord (Maybe a)
-- Ord1 Maybe
-- (Bounded a) => Bounded (Maybe a)
-- (Show a) => Show (Maybe a)

array :: Array Int
array = [1,2,3,4,5,32,23,32]

add :: Number -> Number
add n = n+1.0

main :: Effect Unit
main = do
  log $ "maybe :: forall a b. b -> (a -> b) -> Maybe a -> b"
  logShow $ maybe 11.9 add $ Just 9.3
  log $ "maybe' :: forall a b. (Unit -> b) -> (a -> b) -> Maybe a -> b"
  logShow $ maybe' (\x -> 8.2) add (Just 9.1)
  log $ "fromMaybe :: forall a. a -> Maybe a -> a"
  logShow $ fromMaybe 4.5 $ Just 4.5
  log $ "fromMaybe' :: forall a. (Unit -> a) -> Maybe a -> a"
  logShow $ fromMaybe' (\x -> 4.5) (Just 9.3)
  log $ "isJust :: forall a. Maybe a -> Boolean"
  logShow $ isJust $ Just 4.5
  logShow $ isJust Nothing
  log $ "isNothing :: forall a. Maybe a -> Boolean"
  logShow $ isNothing Nothing
  logShow $ isNothing $ Just 2
  log $ "fromJust :: forall a. Partial => Maybe a -> a"
  logShow $ unsafePartial $ fromJust $ Just 8
  log $ "optional :: forall f a. Alternative f => f a -> f (Maybe a)"
  logShow $ optional array
