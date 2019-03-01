module Main where

import Prelude (class Ord, Ordering, Unit, compare, discard, mod, unit, ($), (+), (==), (>))
import Effect (Effect)
import Effect.Class.Console (logShow, log)
import Data.Array (alterAt, catMaybes, takeWhile, concat, group', concatMap, cons, delete, deleteAt, deleteBy, difference, drop, dropEnd, dropWhile, elemIndex, elemLastIndex, filter, filterA, findIndex, findLastIndex, foldM, foldRecM, fromFoldable, group, groupBy, head, index, init, insert, insertAt, insertBy, intersect, intersectBy, last, length, mapMaybe, mapWithIndex, modifyAt, modifyAtIndices, nub, nubBy, nubByEq, nubEq, null, partition, range, replicate, reverse, singleton, slice, snoc, sort, sortBy, sortWith, span, tail, take, takeEnd, toUnfoldable, uncons, union, unionBy, unsafeIndex, unsnoc, unzip, updateAt, updateAtIndices, zip, zipWith, zipWithA, (!!), (..), (:), (\\))
import Data.List (List)
import Data.Int (fromNumber, even)
import Partial.Unsafe (unsafePartial)
import Data.Tuple (Tuple(..))
import Data.Ordering (invert)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains, toUpper)

-- Alternative -> Instances
-- Alternative Array

-- Lazy -> Instances
-- Lazy (a -> b)
-- Lazy Unit

-- NonEmptyArray
-- Instances
-- (Show a) => Show (NonEmptyArray a)
-- (Eq a) => Eq (NonEmptyArray a)
-- Eq1 NonEmptyArray
-- (Ord a) => Ord (NonEmptyArray a)
-- Ord1 NonEmptyArray
-- Semigroup (NonEmptyArray a)
-- Functor NonEmptyArray
-- FunctorWithIndex Int NonEmptyArray
-- Foldable NonEmptyArray
-- FoldableWithIndex Int NonEmptyArray
-- Foldable1 NonEmptyArray
-- Unfoldable1 NonEmptyArray
-- Traversable NonEmptyArray
-- TraversableWithIndex Int NonEmptyArray
-- Traversable1 NonEmptyArray
-- Apply NonEmptyArray
-- Applicative NonEmptyArray
-- Bind NonEmptyArray
-- Monad NonEmptyArray
-- Alt NonEmptyArray

invertCompare :: forall a. Ord a => a -> a -> Ordering
invertCompare a b = invert $ compare a b

array1 :: Array Unit
array1 = [unit,unit,unit,unit,unit,unit]

add :: Number -> Number -> Number
add a b = a + b

add1 :: Int -> Int -> Maybe Int
add1 a b = Just(a + b)

add2 :: Int -> Int -> Int
add2 i i2 = i+i2

add3 :: Int -> Int
add3 i = i+5

greater :: Int -> Int -> Boolean
greater i i1 = if i > i1 then true else false

arrayint :: Array Int
arrayint = [3,45,2,7678,34,22,34,344,21,42]

arrayin :: Array Int
arrayin = [3,6,5,3,2,667,45,3212,21]

array :: Array Number
array = [1.0,6.0,6.0,9.4,4.37,3.5,3.5,2.43]

array2 :: Array Number
array2 = [3.4,3.4,7.6,32.4,45.345,5.3,43.4]

arraytuple :: Array (Tuple String Int)
arraytuple = [Tuple "m" 4, Tuple "v" 3, Tuple "e" 2, Tuple "a" 5]

arraystr :: Array String
arraystr = ["a", "b", "c", "d", "e"]

arrayar :: Array (Array Int)
arrayar = [[1,2,3,4,5], [10,20,30,40,50], [11,12,13,14,15], [90,80,70,60,50], [0,9,8,7,6]]

arraym :: Array (Maybe Int)
arraym = [Just 3, Just 6, Just 8, Just 9, Just 90, Just 67]

maybeItself :: Int -> Maybe Int
maybeItself i = if i `mod` 2 == 0 then (Just i) else Nothing

maybeBoolean :: Int -> Maybe Boolean
maybeBoolean i = if i `mod` 2 == 0 then (Just true) else (Just false)

main :: Effect Unit
main = do
  log $ "fromFoldable : forall f. Foldable f => f ~> Array"
  logShow $ fromFoldable $ Just array
  log $ "toUnfoldable : forall f. Unfoldable f => Array ~> f"
  logShow $ toUnfoldable array :: List _
  log $ "singleton : forall a. a -> Array a"
  logShow $ singleton "abc"
  log $ "range : Int -> Int -> Array Int"
  logShow $ range 90 4
  log $ "replicate : forall a. Int -> a -> Array a"
  logShow $ replicate 3 "abc"
  log $ "some : forall f a. Alternative f => Lazy (f (Array a)) => f a -> f (Array a)"
  -- logShow $ some arrayint :: Array (Array Int)
  log $ "many : forall f a. Alternative f => Lazy (f (Array a)) => f a -> f (Array a)"
  -- logShow $ many array
  log $ "null : forall a. Array a -> Boolean"
  logShow $ null array
  log $ "length : forall a. Array a -> Int"
  logShow $ length array
  log $ "cons : forall a. a -> Array a -> Array a"
  logShow $ cons 49.76 array
  log $ "snoc : forall a. Array a -> a -> Array a"
  logShow $ snoc array 56.093
  log $ "insert : forall a. Ord a => a -> Array a -> Array a"
  logShow $ insert 5.54 array
  log $ "insertBy : forall a. (a -> a -> Ordering) -> a -> Array a -> Array a"
  logShow $ insertBy invertCompare 4.9 array
  log $ "head : forall a. Array a -> Maybe a"
  logShow $ head array
  log $ "last : forall a. Array a -> Maybe a"
  logShow $ last array
  log $ "tail : forall a. Array a -> Maybe (Array a)"
  logShow $ tail array
  log $ "init : forall a. Array a -> Maybe (Array a)"
  logShow $ init array
  log $ "uncons : forall a. Array a -> Maybe { head :: a, tail :: Array a }"
  logShow $ uncons array
  logShow $ unsnoc array
  log $ "index : forall a. Array a -> Int -> Maybe a"
  logShow $ index array 4
  log $ "elemIndex : forall a. Eq a => a -> Array a -> Maybe Int"
  logShow $ elemIndex 3.5 array
  log $ "elemLastIndex : forall a. Eq a => a -> Array a -> Maybe Int"
  logShow $ elemLastIndex 3.5 array
  log $ "findIndex : forall a. (a -> Boolean) -> Array a -> Maybe Int"
  logShow $ findIndex (contains (Pattern "c")) arraystr
  log $ "findLastIndex : forall a. (a -> Boolean) -> Array a -> Maybe Int"
  logShow $ findLastIndex (contains (Pattern "a")) arraystr
  log $ "insertAt : forall a. Int -> a -> Array a -> Maybe (Array a)"
  logShow $ insertAt 3 3.5 array
  log $ "deleteAt : forall a. Int -> Array a -> Maybe (Array a)"
  logShow $ deleteAt 4 array
  log $ "updateAt : forall a. Int -> a -> Array a -> Maybe (Array a)"
  logShow $ updateAt 2 4.5 array
  log $ "updateAtIndices : forall t a. Foldable t => t (Tuple Int a) -> Array a -> Array a"
  logShow $ updateAtIndices [Tuple 1 "a"] arraystr
  log $ "modifyAt : forall a. Int -> (a -> a) -> Array a -> Maybe (Array a)"
  logShow $ modifyAt 4 toUpper arraystr
  log $ "modifyAtIndices : forall t a. Foldable t => t Int -> (a -> a) -> Array a -> Array a"
  logShow $ modifyAtIndices [1, 3] toUpper arraystr
  log $ "alterAt : forall a. Int -> (a -> Maybe a) -> Array a -> Maybe (Array a)"
  logShow $ alterAt 3 maybeItself arrayint
  log $ "reverse : forall a. Array a -> Array a"
  logShow $ reverse array
  log $ "concat : forall a. Array (Array a) -> Array a"
  logShow $ concat arrayar
  log $ "concatMap : forall a b. (a -> Array b) -> Array a -> Array b"
  logShow $ concatMap singleton array
  log $ "filter : forall a. (a -> Boolean) -> Array a -> Array a"
  logShow $ filter (contains (Pattern "b")) arraystr
  log $ "partition : forall a. (a -> Boolean) -> Array a -> { yes :: Array a, no :: Array a }"
  logShow $ partition (contains (Pattern "c")) arraystr
  log $ "filterA : forall a f. Applicative f => (a -> f Boolean) -> Array a -> f (Array a)"
  logShow $ filterA maybeBoolean arrayint
  log $ "mapMaybe : forall a b. (a -> Maybe b) -> Array a -> Array b"
  logShow $ mapMaybe fromNumber array
  log $ "catMaybes : forall a. Array (Maybe a) -> Array a"
  logShow $ catMaybes arraym
  log $ "mapWithIndex : forall a b. (Int -> a -> b) -> Array a -> Array b"
  logShow $ mapWithIndex add2 arrayint
  log $ "sortBy : forall a. (a -> a -> Ordering) -> Array a -> Array a"
  logShow $ sortBy invertCompare array
  log $ "sort : forall a. Ord a => Array a -> Array a"
  logShow $ sort array
  log $ "sortWith : forall a b. Ord b => (a -> b) -> Array a -> Array a"
  logShow $ sortWith add3 arrayint
  log $ "slice : forall a. Int -> Int -> Array a -> Array a"
  logShow $ slice 1 4 array
  log $ "take : forall a. Int -> Array a -> Array a"
  logShow $ take 3 array
  log $ "takeEnd : forall a. Int -> Array a -> Array a"
  logShow $ takeEnd 3 array
  log $ "takeWhile : forall a. (a -> Boolean) -> Array a -> Array a"
  logShow $ takeWhile even arrayint
  log $ "drop : forall a. Int -> Array a -> Array a"
  logShow $ drop 2 array
  log $ "dropEnd : forall a. Int -> Array a -> Array a"
  logShow $ dropEnd 3 array
  log $ "dropWhile : forall a. (a -> Boolean) -> Array a -> Array a"
  logShow $ dropWhile (contains (Pattern "d")) arraystr
  log $ "span : forall a. (a -> Boolean) -> Array a -> { init :: Array a, rest :: Array a }"
  logShow $ span (contains (Pattern "c")) arraystr
  log $ "group : forall a. Eq a => Array a -> Array (NonEmptyArray a)"
  logShow $ group array
  log $ "group' : forall a. Ord a => Array a -> Array (NonEmptyArray a)"
  logShow $ group' array
  log $ "groupBy : forall a. (a -> a -> Boolean) -> Array a -> Array (NonEmptyArray a)"
  logShow $ groupBy greater arrayint
  log $ "nub : forall a. Ord a => Array a -> Array a"
  logShow $ nub array
  log $ "nubEq : forall a. Eq a => Array a -> Array a"
  logShow $ nubEq array
  log $ "nubBy : forall a. (a -> a -> Ordering) -> Array a -> Array a"
  logShow $ nubBy invertCompare array
  log $ "nubByEq : forall a. (a -> a -> Boolean) -> Array a -> Array a"
  logShow $ nubByEq greater arrayint
  log $ "union : forall a. Eq a => Array a -> Array a -> Array a"
  logShow $ union array array2
  log $ "unionBy : forall a. (a -> a -> Boolean) -> Array a -> Array a -> Array a"
  logShow $ unionBy greater arrayint arrayin
  log $ "delete : forall a. Eq a => a -> Array a -> Array a"
  logShow $ delete 3.5 array
  log $ "deleteBy : forall a. (a -> a -> Boolean) -> a -> Array a -> Array a"
  logShow $ deleteBy greater 5 arrayint
  log $ "difference : forall a. Eq a => Array a -> Array a -> Array a"
  logShow $ difference array array2
  log $ "intersect : forall a. Eq a => Array a -> Array a -> Array a"
  logShow $ intersect array array2
  log $ "intersectBy : forall a. (a -> a -> Boolean) -> Array a -> Array a -> Array a"
  logShow $ intersectBy greater arrayint arrayin
  log $ "zipWith : forall a b c. (a -> b -> c) -> Array a -> Array b -> Array c"
  logShow $ zipWith add array array2
  log $ "zipWithA : forall m a b c. Applicative m => (a -> b -> m c) -> Array a -> Array b -> m (Array c)"
  logShow $ zipWithA add1 arrayin arrayint
  log $ "zip : forall a b. Array a -> Array b -> Array (Tuple a b)"
  logShow $ zip arraystr array
  log $ "unzip : forall a b. Array (Tuple a b) -> Tuple (Array a) (Array b)"
  logShow $ unzip arraytuple
  log $ "foldM : forall m a b. Monad m => (a -> b -> m a) -> a -> Array b -> m a"
  logShow $ foldM add1 5 arrayint
  log $ "foldRecM : forall m a b. MonadRec m => (a -> b -> m a) -> a -> Array b -> m a"
  logShow $ foldRecM add1 4 arrayint
  log $ "unsafeIndex : forall a. Partial => Array a -> Int -> a"
  logShow $ unsafePartial $ unsafeIndex arrayint 4
  log $ "------------ Operators -------------"
  logShow $ 4.37 : array -- Operator alias for Data.Array.cons (right-associative / precedence 6)
  logShow $ array !! 2 -- Operator alias for Data.Array.index (left-associative / precedence 8)
  logShow $ array \\ array2 -- Operator alias for Data.Array.difference (non-associative / precedence 5)
  logShow $ 3..19 -- Operator alias for Data.Array.NonEmpty.range (non-associative / precedence 8)
