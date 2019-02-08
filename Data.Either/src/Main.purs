module Main where

import Prelude (Unit, discard, ($))
import Effect (Effect)
import Data.Either (Either(..), either, choose, isLeft, isRight, fromLeft, fromRight, note, note', hush)
import Partial.Unsafe (unsafePartial)
import Control.Alt (class Alt)
import Math (floor, trunc)
import Data.Maybe (Maybe(..))
import Effect.Class.Console (logShow)

-- Instances
-- Functor (Either a)
-- FunctorWithIndex Unit (Either a)
-- Invariant (Either a)
-- Bifunctor Either
-- Apply (Either e)
-- Applicative (Either e)
-- Alt (Either e)
-- Bind (Either e)
-- Monad (Either e)
-- Extend (Either e)
-- (Show a, Show b) => Show (Either a b)
-- (Eq a, Eq b) => Eq (Either a b)
-- (Eq a) => Eq1 (Either a)
-- (Ord a, Ord b) => Ord (Either a b)
-- (Ord a) => Ord1 (Either a)
-- (Bounded a, Bounded b) => Bounded (Either a b)
-- Foldable (Either a)
-- FoldableWithIndex Unit (Either a)
-- Bifoldable Either
-- Traversable (Either a)
-- TraversableWithIndex Unit (Either a)
-- Bitraversable Either
-- (Semigroup b) => Semigroup (Either a b)

either1 :: forall a b c. (a -> c) -> (b -> c) -> Either a b -> c
either1 a b c = either a b c

choose1 :: forall m a b. Alt m => m a -> m b -> m (Either a b)
choose1 a b = choose a b

isLeft1 :: forall a b. Either a b -> Boolean
isLeft1 a = isLeft a

isRight1 :: forall a b. Either a b -> Boolean
isRight1 a = isRight a

fromLeft1 :: forall a b. Partial => Either a b -> a
fromLeft1 a = fromLeft a

fromRight1 :: forall a b. Partial => Either a b -> b
fromRight1 a = fromRight a

note1 :: forall a b. a -> Maybe b -> Either a b
note1 a b = note a b

note'1 :: forall a b. (Unit -> a) -> Maybe b -> Either a b
note'1 unit m = note' unit m

hush1 :: forall a b. Either a b -> Maybe b
hush1 a = hush a

main :: Effect Unit
main = do
  logShow $ either1 trunc floor (Left 3.4)
  logShow $ choose1 (Just 10) (Just 9)
  logShow $ isLeft1 (Left 9)
  logShow $ isRight1 (Right 3)
  logShow $ unsafePartial $ fromLeft1 (Left 9)
  logShow $ unsafePartial $ fromRight1 (Right 5)
  logShow $ note1 2 (Just 10)
  -- logShow $ note' (\x -> x+x) (Just 8)
  logShow $ hush1 (Right 8) :: Maybe Int
