module Main where

import Prelude
import Effect (Effect)
import Effect.Class.Console (logShow)
import Data.Maybe
import Control.Alternative
import Partial.Unsafe (unsafePartial)
import Math

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

maybe1 :: forall a b. b -> (a -> b) -> Maybe a -> b
maybe1 b f j = maybe b f j

maybe'1 :: forall a b. (Unit -> b) -> (a -> b) -> Maybe a -> b
maybe'1 u f m = maybe' u f m

fromMaybe1 :: forall a. a -> Maybe a -> a
fromMaybe1 a j = fromMaybe a j

fromMaybe'1 :: forall a. (Unit -> a) -> Maybe a -> a
fromMaybe'1 u m = fromMaybe' u m

isJust1 :: forall a. Maybe a -> Boolean
isJust1 j = isJust j

isNothing1 :: forall a. Maybe a -> Boolean
isNothing1 j = isNothing j

fromJust1 :: forall a. Partial => Maybe a -> a
fromJust1 j = fromJust j

optional1 :: forall f a. Alternative f => f a -> f (Maybe a)
optional1 f = optional f

main :: Effect Unit
main = do
  logShow $ maybe1 11.9 add $ Just 9.3
  logShow $ maybe'1 (\x -> 8.2) add (Just 9.1)
  logShow $ fromMaybe1 4.5 $ Just 4.5
  logShow $ fromMaybe'1 (\x -> 4.5) (Just 9.3)
  logShow $ isJust1 $ Just 4.5
  logShow $ isJust1 Nothing
  logShow $ isNothing1 Nothing
  logShow $ isNothing1 $ Just 2
  logShow $ unsafePartial $ fromJust1 $ Just 8
  logShow $ optional1 array
