module Main where

import Prelude (class Applicative, class Eq, class HeytingAlgebra, class Monad, class Monoid, class Ord, class Semigroup, class Semiring, Ordering, Unit, compare, discard, not, show, ($), (+))
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Data.Foldable (class Foldable, all, and, any, elem, find, findMap, fold, foldM, foldMapDefaultL, foldMapDefaultR, foldlDefault, foldrDefault, for_, indexl, indexr, intercalate, length, maximum, maximumBy, minimum, minimumBy, notElem, null, oneOf, oneOfMap, or, product, sequence_, sum, surround, surroundMap, traverse_)
import Data.Maybe (Maybe(..))
import Data.Int (even, fromNumber, round, toNumber)
import Control.Plus (class Plus)
import Data.Ordering (invert)

-- Instances
-- Foldable Array
-- Foldable Maybe
-- Foldable First
-- Foldable Last
-- Foldable Additive
-- Foldable Dual
-- Foldable Disj
-- Foldable Conj
-- Foldable Multiplicative

add :: Number -> Int -> Int
add n i = (round n) + i

sum2 :: Number -> Int -> Number
sum2 n i = n + toNumber i

add1 :: Int -> Int -> Maybe Int
add1 a b = Just(a + b)

add2 :: Int -> Array (Maybe Int)
add2 i = [Just (i+1)]

arrays :: Int -> Array Int
arrays i = [i+1,i+2,i+3]

invertCompare :: forall a. Ord a => a -> a -> Ordering
invertCompare a b = invert $ compare a b

arraystr :: Array String
arraystr = ["a", "b", "c", "d", "e"]

arraym1 :: Array (Array (Maybe Int))
arraym1 = [[(Just 4)],[(Just 6),(Just 67),(Just 98)],[(Just 4),(Just 22),(Just 48),(Just 49)]]

arraym :: Array (Maybe Int)
arraym = [(Just 4),(Just 6),(Just 67),(Just 98),(Just 4),(Just 22),(Just 48),(Just 49)]

array :: Array Number
array = [1.55, 7.8, 9.9, 6.8, 9.9, 0.3, 99.0]

arrayint :: Array Int
arrayint = [1,23,4,5,6,7,8,9,8,8,80,54,33]

arrayarr :: Array (Array Number)
arrayarr = [[1.3,7.3,94.5,0.6,3.5,44.3,3.3],[7.2,33.22,21.5,33.6,29.56,2.67],[21.54,43.4,44.33,67.55,67.76,76.00]]

main :: Effect Unit
main = do
  log $ "foldrDefault :: forall f a b. Foldable f => (a -> b -> b) -> b -> f a -> b"
  logShow $ foldrDefault add 6 array
  log $ "foldlDefault :: forall f a b. Foldable f => (b -> a -> b) -> b -> f a -> b"
  logShow $ foldlDefault sum2 9.6 arrayint
  log $ "foldMapDefaultL :: forall f a m. Foldable f => Monoid m => (a -> m) -> f a -> m"
  logShow $ foldMapDefaultL add2 arrayint
  log $ "foldMapDefaultR :: forall f a m. Foldable f => Monoid m => (a -> m) -> f a -> m"
  logShow $ foldMapDefaultR add2 arrayint
  log $ "fold :: forall f m. Foldable f => Monoid m => f m -> m"
  logShow $ fold arraym1
  log $ "foldM :: forall f m a b. Foldable f => Monad m => (a -> b -> m a) -> a -> f b -> m a"
  logShow $ foldM add1 4 arrayint
  log $ "traverse_ :: forall a b f m. Applicative m => Foldable f => (a -> m b) -> f a -> m Unit"
  logShow $ traverse_ fromNumber array
  log $ "for_ :: forall a b f m. Applicative m => Foldable f => f a -> (a -> m b) -> m Unit"
  logShow $ for_ array fromNumber
  log $ "sequence_ :: forall a f m. Applicative m => Foldable f => f (m a) -> m Unit"
  logShow $ sequence_ arraym
  log $ "oneOf :: forall f g a. Foldable f => Plus g => f (g a) -> g a"
  logShow $ oneOf arrayarr
  log $ "oneOfMap :: forall f g a b. Foldable f => Plus g => (a -> g b) -> f a -> g b"
  logShow $ oneOfMap arrays arrayint
  log $ "intercalate :: forall f m. Foldable f => Monoid m => m -> f m -> m"
  logShow $ intercalate "," arraystr
  log $ "surroundMap :: forall f a m. Foldable f => Semigroup m => m -> (a -> m) -> f a -> m"
  logShow $ surroundMap "u" show array
  log $ "surround :: forall f m. Foldable f => Semigroup m => m -> f m -> m"
  logShow $ surround "*" arraystr
  log $ "and :: forall a f. Foldable f => HeytingAlgebra a => f a -> a"
  logShow $ and [true,false,true,true,false,false]
  log $ "or :: forall a f. Foldable f => HeytingAlgebra a => f a -> a"
  logShow $ or [true,false,true,true,false,false]
  log $ "all :: forall a b f. Foldable f => HeytingAlgebra b => (a -> b) -> f a -> b"
  logShow $ all not [true,false,true,true,false,false]
  log $ "any :: forall a b f. Foldable f => HeytingAlgebra b => (a -> b) -> f a -> b"
  logShow $ any not [true,false,true,true,false,false]
  log $ "sum :: forall a f. Foldable f => Semiring a => f a -> a"
  logShow $ sum array
  log $ "product :: forall a f. Foldable f => Semiring a => f a -> a"
  logShow $ product array
  log $ "elem :: forall a f. Foldable f => Eq a => a -> f a -> Boolean"
  logShow $ element 1.0 array
  log $ "notElem :: forall a f. Foldable f => Eq a => a -> f a -> Boolean"
  logShow $ notElement 2.33 array
  log $ "indexl :: forall a f. Foldable f => Int -> f a -> Maybe a"
  logShow $ indexl 4 array
  log $ "indexr :: forall a f. Foldable f => Int -> f a -> Maybe a"
  logShow $ indexr 3 array
  log $ "find :: forall a f. Foldable f => (a -> Boolean) -> f a -> Maybe a"
  logShow $ find even arrayint
  log $ "findMap :: forall a b f. Foldable f => (a -> Maybe b) -> f a -> Maybe b"
  logShow $ findMap fromNumber array
  log $ "maximum :: forall a f. Ord a => Foldable f => f a -> Maybe a"
  logShow $ maximum array
  log $ "maximumBy :: forall a f. Foldable f => (a -> a -> Ordering) -> f a -> Maybe a"
  logShow $ maximumBy invertCompare array
  log $ "minimum :: forall a f. Ord a => Foldable f => f a -> Maybe a"
  logShow $ minimum array
  log $ "minimumBy :: forall a f. Foldable f => (a -> a -> Ordering) -> f a -> Maybe a"
  logShow $ minimumBy invertCompare array
  log $ "null :: forall a f. Foldable f => f a -> Boolean"
  logShow $ null array
  log $ "length :: forall a b f. Foldable f => Semiring b => f a -> b"
  logShow $ length arrayint :: Int
