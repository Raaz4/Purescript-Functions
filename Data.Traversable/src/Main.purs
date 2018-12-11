module Main where

import Prelude
import Effect (Effect)
import Effect.Class.Console (logShow)
import Data.Traversable
import Data.Int

traverseDefault1 :: forall t a b m. Traversable t => Applicative m => (a -> m b) -> t a -> m (t b)
traverseDefault1 m t = traverseDefault m t

sequenceDefault1 :: forall t a m. Traversable t => Applicative m => t (m a) -> m (t a)
sequenceDefault1 tm = sequenceDefault tm

for1 :: forall a b m t. Applicative m => Traversable t => t a -> (a -> m b) -> m (t b)
for1 t f = for t f

scanl1 :: forall a b f. Traversable f => (b -> a -> b) -> b -> f a -> f b
scanl1 f b array scanl f b array

scanr1 :: forall a b f. Traversable f => (a -> b -> b) -> b -> f a -> f b
scanr1 f b array = scanr f b array

mapAccumL1 :: forall a b s f. Traversable f => (s -> a -> Accum s b) -> s -> f a -> Accum s (f b)
mapAccumL1 f s array = mapAccumL f s array

mapAccumR1 :: forall a b s f. Traversable f => (s -> a -> Accum s b) -> s -> f a -> Accum s (f b)
mapAccumR1 f s array = mapAccumR f s array

main :: Effect Unit
main = do
  -- logShow $ traverseDefault1 fromNumber
