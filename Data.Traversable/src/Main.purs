module Main where

import Prelude (Unit, discard, ($), (+))
import Effect (Effect)
import Effect.Class.Console (logShow, log)
import Data.Traversable (for, mapAccumL, mapAccumR, scanl, scanr, sequenceDefault, traverseDefault)
import Data.Maybe (Maybe(..))
import Data.Int (fromNumber)

-- Instances
-- Traversable Array
-- Traversable Maybe
-- Traversable First
-- Traversable Last
-- Traversable Additive
-- Traversable Dual
-- Traversable Conj
-- Traversable Disj
-- Traversable Multiplicative

-- accum :: forall s a. s -> a -> Accum s a
-- accum s a = Accum s a

first :: forall a b. b -> a -> b
first b a = b

second :: forall a b. a -> b -> b
second a b = b

arrayint :: Array Int
arrayint = [1,2,4,32,54,312,32,342]

arraynum :: Array Number
arraynum = [1.44,3.22,43.22,43.22,5.43,33.32]

arraym :: Array (Maybe Int)
arraym = [(Just 4), (Just 5), (Just 90), (Just 45), (Just 8)]

main :: Effect Unit
main = do
  log $ "traverseDefault :: forall t a b m. Traversable t => Applicative m => (a -> m b) -> t a -> m (t b)"
  logShow $ traverseDefault fromNumber arraynum
  log $ "sequenceDefault :: forall t a m. Traversable t => Applicative m => t (m a) -> m (t a)"
  logShow $ sequenceDefault arraym
  log $ "for :: forall a b m t. Applicative m => Traversable t => t a -> (a -> m b) -> m (t b)"
  logShow $ for arraynum fromNumber
  log $ "mapAccumL :: forall a b s f. Traversable f => (s -> a -> Accum s b) -> s -> f a -> Accum s (f b)"
  logShow $ mapAccumL (\s a -> {accum: s+1, value: s+a}) 5 arrayint
  log $ "mapAccumR :: forall a b s f. Traversable f => (s -> a -> Accum s b) -> s -> f a -> Accum s (f b)"
  logShow $ mapAccumR (\s a -> {accum: s, value: a}) 6 arrayint
  log $ "scanl :: forall a b f. Traversable f => (b -> a -> b) -> b -> f a -> f b"
  logShow $ scanl first 4 arrayint
  log $ "scanr :: forall a b f. Traversable f => (a -> b -> b) -> b -> f a -> f b"
  logShow $ scanr second 8 arrayint
