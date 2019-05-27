module Main where

import Control.Alt (class Alt)
import Data.Either (Either(..), either, choose, isLeft, isRight, fromLeft, fromRight, note, note', hush)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Math (floor, trunc)
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, discard, ($))

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

main :: Effect Unit
main = do
  log $ "choose : forall m a b. Alt m => m a -> m b -> m (Either a b)"
  logShow $ choose (Just 10) (Just 9)
  log $ "either : forall a b c. (a -> c) -> (b -> c) -> Either a b -> c"
  logShow $ either trunc floor (Left 3.4)
  log $ "fromLeft : forall a b. Partial => Either a b -> a"
  logShow $ unsafePartial $ fromLeft1 (Left 9)
  log $ "fromRight : forall a b. Partial => Either a b -> b"
  logShow $ unsafePartial $ fromRight1 (Right 5)
  log $ "hush :: forall a b. Either a b -> Maybe b"
  logShow $ hush (Right 8) :: Maybe Int
  log $ "isLeft : forall a b. Either a b -> Boolean"
  logShow $ isLeft (Left 9)
  log $ "isRight : forall a b. Either a b -> Boolean"
  logShow $ isRight (Right 3)
  log $ "note : forall a b. a -> Maybe b -> Either a b"
  logShow $ note 2 (Just 10)
  log $ "note' : forall a b. (Unit -> a) -> Maybe b -> Either a b"
  -- logShow $ note' (\x -> x+x) (Just 8)
  