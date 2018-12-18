module Main where

import Prelude
import Effect (Effect)
import Effect.Class.Console (logShow)
import Data.Array
import Data.Array (singleton, range, replicate) as A
import Data.Foldable
import Data.Unfoldable
import Data.Maybe
import Data.String

array :: Array Number
array = [1.0,6.0,9.4,4.37,3.5,2.43]

arraystr :: Array String
arraystr = ["a", "b", "c", "d", "e"]

arrayar :: Array (Array Int)
arrayar = [[1,2,3,4,5], [10,20,30,40,50], [11,12,13,14,15], [90,80,70,60,50], [0,9,8,7,6]]

fromFoldable1 ::  forall f. Foldable f => f ~> Array
fromFoldable1 f = fromFoldable f

toUnfoldable1 :: forall f. Unfoldable f => Array ~> f
toUnfoldable1 array = toUnfoldable array

singleton1 :: forall a. a -> Array a
singleton1 a = A.singleton a

range1 :: Int -> Int -> Array Int
range1 f t = A.range f t

replicate1 :: forall a. Int -> a -> Array a
replicate1 n s = A.replicate n s

cons1 :: forall a. a -> Array a -> Array a
cons1 a array = cons a array

snoc1 :: forall a. Array a -> a -> Array a
snoc1 array a = snoc array a

insert1 :: forall a. Ord a => a -> Array a -> Array a
insert1 a array = insert a array

head1 :: forall a. Array a -> Maybe a
head1 array = head array

tail1 :: forall a. Array a -> Maybe (Array a)
tail1 array = tail array

init1 :: forall a. Array a -> Maybe (Array a)
init1 array = init array

findIndex1 :: forall a. (a -> Boolean) -> Array a -> Maybe Int
findIndex1 a b = findIndex a b

concatinate :: forall a. Array (Array a) -> Array a
concatinate array = concat array

concatMap1 :: forall a b. (a -> Array b) -> Array a -> Array b
concatMap1 array1 array2 = concatMap array1 array2

main :: Effect Unit
main = do
  logShow $ fromFoldable1 $ Just array
  logShow $ toUnfoldable $ fromFoldable1 array
  logShow $ singleton1 "abc"
  logShow $ range1 90 4
  logShow $ replicate1 3 "abc"
  logShow $ cons1 49.76 array
  logShow $ snoc1 array 56.093
  logShow $ insert1 5.54 array
  logShow $ head1 array
  logShow $ tail1 array
  logShow $ init1 array
  logShow $ findIndex1 (contains (Pattern "c")) arraystr
  logShow $ concatinate arrayar
  logShow $ concatMap1 singleton1 array
  logShow $ 4.37 : array -- cons -> :
