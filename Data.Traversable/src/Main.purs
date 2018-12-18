module Main where

import Prelude
import Effect (Effect)
import Effect.Class.Console (logShow)
import Data.Traversable
import Data.Maybe
import Data.Maybe.Last
import Data.Tuple
import Data.Monoid.Multiplicative
import Data.Int

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

first :: forall a b. b -> a -> b
first b a = b

second :: forall a b. a -> b -> b
second a b = b

arrayint :: Array Int
arrayint = [1,2,4,32,54,312,32,342]

arraynum :: Array Number
arraynum = [1.44,3.22,43.22,43.22,5.43,33.32]

arraym :: Array (Maybe Int)
arraym = [Just 4, Just 5, Just 90, Just 45, Just 8]

traverseDefault1 :: forall t a b m. Traversable t => Applicative m => (a -> m b) -> t a -> m (t b)
traverseDefault1 m t = traverseDefault m t

sequenceDefault1 :: forall t a m. Traversable t => Applicative m => t (m a) -> m (t a)
sequenceDefault1 tm = sequenceDefault tm

for1 :: forall a b m t. Applicative m => Traversable t => t a -> (a -> m b) -> m (t b)
for1 t f = for t f

scanl1 :: forall a b f. Traversable f => (b -> a -> b) -> b -> f a -> f b
scanl1 f b array = scanl f b array

scanr1 :: forall a b f. Traversable f => (a -> b -> b) -> b -> f a -> f b
scanr1 f b array = scanr f b array

mapAccumL1 :: forall a b s f. Traversable f => (s -> a -> Accum s b) -> s -> f a -> Accum s (f b)
mapAccumL1 f s array = mapAccumL f s array

mapAccumR1 :: forall a b s f. Traversable f => (s -> a -> Accum s b) -> s -> f a -> Accum s (f b)
mapAccumR1 f s array = mapAccumR f s array

main :: Effect Unit
main = do
  logShow $ traverseDefault1 fromNumber arraynum
  logShow $ sequenceDefault1 arraym
  logShow $ for1 arraynum fromNumber
  -- logShow $ Accum 8 3
  logShow $ scanl1 first 4 arrayint
  logShow $ scanr1 second 8 arrayint
