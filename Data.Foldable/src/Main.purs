module Main where

import Prelude
import Effect (Effect)
import Effect.Class.Console (logShow)
import Data.Foldable (null,length)
import Data.Foldable
import Math (max) as M
import Data.Maybe
import Data.String
import Data.Int (round)
import Data.Int
import Math (ceil)
import Math
import Data.Tuple
import Debug.Trace

add :: Number -> Int -> Int
add n i = (round n) + i

add1 :: Int -> Int -> Maybe Int
add1 a b = Just(a + b)

add2 :: Int -> Array (Maybe Int)
add2 i = [Just (i+1)]

sub a b = b - a
mul a b = a * b
div a b = a / b

arraystr = ["a", "b", "c", "d", "e"]

even1 :: Int -> Boolean
even1 e = even e

arraym1 = [[(Just 4)],[(Just 6),(Just 67),(Just 98)],[(Just 4),(Just 22),(Just 48),(Just 49)]]

arraym = [(Just 4),(Just 6),(Just 67),(Just 98),(Just 4),(Just 22),(Just 48),(Just 49)]
array = [1.55, 7.8, 9.9, 6.8, 9.9, 0.3, 99.0]
arrayint = [1,23,4,5,6,7,8,9,8,8,80,54,33]
arrayarr = [[1.3,7.3,94.5,0.6,3.5,44.3,3.3],[7.2,33.22,21.5,33.6,29.56,2.67],[21.54,43.4,44.33,67.55,67.76,76.00]]

foldrDefault1 :: forall f a b. Foldable f => (a -> b -> b) -> b -> f a -> b
foldrDefault1 a b c = foldrDefault a b c

foldMapDefaultL1 :: forall f a m. Foldable f => Monoid m => (a -> m) -> f a -> m
foldMapDefaultL1 a b = foldMapDefaultL a b

foldMapDefaultR1 :: forall f a m. Foldable f => Monoid m => (a -> m) -> f a -> m
foldMapDefaultR1 a b = foldMapDefaultR a b

fold1 :: forall f m. Foldable f => Monoid m => f m -> m
fold1 a = fold a

foldM1 :: forall f m a b. Foldable f => Monad m => (a -> b -> m a) -> a -> f b -> m a
foldM1 a b c = foldM a b c

traverse1 :: forall a b f m. Applicative m => Foldable f => (a -> m b) -> f a -> m Unit
traverse1 a b = traverse_ a b

for1 :: forall a b f m. Applicative m => Foldable f => f a -> (a -> m b) -> m Unit
for1 a b = for_ a b

sequence :: forall a f m. Applicative m => Foldable f => f (m a) -> m Unit
sequence a = sequence_ a

intercalate1 :: forall f m. Foldable f => Monoid m => m -> f m -> m
intercalate1 a b = intercalate a b

surround1 :: forall f m. Foldable f => Semigroup m => m -> f m -> m
surround1 a b = surround a b

and1 :: forall a f. Foldable f => HeytingAlgebra a => f a -> a
and1 a = and a

or1 :: forall a f. Foldable f => HeytingAlgebra a => f a -> a
or1 a = or a

all1 :: forall a b f. Foldable f => HeytingAlgebra b => (a -> b) -> f a -> b
all1 a b = all a b

any1 :: forall a b f. Foldable f => HeytingAlgebra b => (a -> b) -> f a -> b
any1 a b = any a b

sum1 :: forall a f. Foldable f => Semiring a => f a -> a
sum1 a = sum a

product1 :: forall a f. Foldable f => Semiring a => f a -> a
product1 a = product a

element :: forall a f. Foldable f => Eq a => a -> f a -> Boolean
element a b = elem a b

notElement :: forall a f. Foldable f => Eq a => a -> f a -> Boolean
notElement a b = notElem a b

indexleft :: forall a f. Foldable f => Int -> f a -> Maybe a
indexleft a b = indexl a b

indexright :: forall a f. Foldable f => Int -> f a -> Maybe a
indexright a b = indexr a b

find1 :: forall a f. Foldable f => (a -> Boolean) -> f a -> Maybe a
find1 a b = find a b

findMap1 :: forall a b f. Foldable f => (a -> Maybe b) -> f a -> Maybe b
findMap1 a b = findMap a b

maximum1 :: forall a f. Ord a => Foldable f => f a -> Maybe a
maximum1 array = maximum array

maximumBy1 :: forall a f. Foldable f => (a -> a -> Ordering) -> f a -> Maybe a
maximumBy1 a b = maximumBy a b

minimum1 :: forall a f. Ord a => Foldable f => f a -> Maybe a
minimum1 a = minimum a

minimumBy1 :: forall a f. Foldable f => (a -> a -> Ordering) -> f a -> Maybe a
minimumBy1 a b = minimumBy a b

null1 :: forall a f. Foldable f => f a -> Boolean
null1 a = null a

length1 :: forall a b f. Foldable f => Semiring b => f a -> b
length1 a = length a

main :: Effect Unit
main = do
  logShow $ foldrDefault1 add 6 array
  logShow $ foldMapDefaultL1 add2 arrayint
  logShow $ foldMapDefaultR1 add2 arrayint
  logShow $ fold1 arraym1
  logShow $ foldM1 add1 4 arrayint
  logShow $ traverse1 fromNumber array
  logShow $ for1 array fromNumber
  logShow $ sequence arraym
  logShow $ intercalate1 "," arraystr
  logShow $ surround1 "*" arraystr
  logShow $ and1 [true,false,true,true,false,false]
  logShow $ or1 [true,false,true,true,false,false]
  logShow $ all1 not [true,false,true,true,false,false]
  logShow $ any1 not [true,false,true,true,false,false]
  logShow $ sum1 array
  logShow $ product1 array
  logShow $ element 1.0 array
  logShow $ notElement 2.33 array
  logShow $ indexleft 4 array
  logShow $ indexright 3 array
  logShow $ find1 even1 arrayint
  logShow $ findMap1 fromNumber array
  logShow $ maximum1 array
  logShow $ minimum1 array
  logShow $ null array
  -- logShow $ length1 arrayint
