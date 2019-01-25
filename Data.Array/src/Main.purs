module Main where

import Prelude
import Effect (Effect)
import Effect.Class.Console (logShow)
import Data.Array
import Data.List
import Data.Array (singleton, range, replicate, null, length, uncons, take, drop, dropWhile, foldM, toUnfoldable)
import Data.Foldable
import Control.Lazy
import Control.Monad.Rec.Class
import Data.Array.NonEmpty.Internal
import Data.Int
import Partial.Unsafe (unsafePartial)
-- import Prim.Partial
import Data.Tuple
import Data.Ordering
import Control.Alternative
import Data.Unfoldable
import Data.Maybe
import Data.String
import Data.String (Pattern(..))

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
arrayint = [2,45,2,7678,34,22,34,344,21,42]

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
maybeItself i = if i `mod` 2 == 0 then (Just i) else (Just (i+1))

maybeBoolean :: Int -> Maybe Boolean
maybeBoolean i = if i `mod` 2 == 0 then (Just true) else (Just false)

fromFoldable1 ::  forall f. Foldable f => f ~> Array
fromFoldable1 f = fromFoldable f

toUnfoldable1 :: forall f. Unfoldable f => Array ~> f
toUnfoldable1 array = toUnfoldable array

singleton1 :: forall a. a -> Array a
singleton1 a = singleton a

range1 :: Int -> Int -> Array Int
range1 f t = range f t

replicate1 :: forall a. Int -> a -> Array a
replicate1 n s = replicate n s

some1 :: forall f a. Alternative f => Lazy (f (Array a)) => f a -> f (Array a)
some1 f = some f

many1 :: forall f a. Alternative f => Lazy (f (Array a)) => f a -> f (Array a)
many1 f = many f

null1 :: forall a. Array a -> Boolean
null1 array = null array

length1 :: forall a. Array a -> Int
length1 array = length array

cons1 :: forall a. a -> Array a -> Array a
cons1 a array = cons a array

snoc1 :: forall a. Array a -> a -> Array a
snoc1 array a = snoc array a

insert1 :: forall a. Ord a => a -> Array a -> Array a
insert1 a array = insert a array

insertBy1 :: forall a. (a -> a -> Ordering) -> a -> Array a -> Array a
insertBy1 o a array = insertBy o a array

head1 :: forall a. Array a -> Maybe a
head1 array = head array

last1 :: forall a. Array a -> Maybe a
last1 array = last array

tail1 :: forall a. Array a -> Maybe (Array a)
tail1 array = tail array

init1 :: forall a. Array a -> Maybe (Array a)
init1 array = init array

uncons1 :: forall a. Array a -> Maybe {head :: a, tail :: Array a}
uncons1 array = uncons array

unsnoc1 :: forall a. Array a -> Maybe {init :: Array a, last :: a}
unsnoc1 array = unsnoc array

index1 :: forall a. Array a -> Int -> Maybe a
index1 array i = index array i

elemIndex1 :: forall a. Eq a => a -> Array a -> Maybe Int
elemIndex1 a array = elemIndex a array

elemLastIndex1 :: forall a. Eq a => a -> Array a -> Maybe Int
elemLastIndex1 a array = elemLastIndex a array

findIndex1 :: forall a. (a -> Boolean) -> Array a -> Maybe Int
findIndex1 a b = findIndex a b

findLastIndex1 :: forall a. (a -> Boolean) -> Array a -> Maybe Int
findLastIndex1 b array = findLastIndex b array

insertAt1 :: forall a. Int -> a -> Array a -> Maybe (Array a)
insertAt1 i a array = insertAt i a array

deleteAt1 :: forall a. Int -> Array a -> Maybe (Array a)
deleteAt1 i array = deleteAt i array

updateAt1 :: forall a. Int -> a -> Array a -> Maybe (Array a)
updateAt1 i a array = updateAt i a array

updateAtIndices1 :: forall t a. Foldable t => t (Tuple Int a) -> Array a -> Array a
updateAtIndices1 t array = updateAtIndices t array

modifyAt1 :: forall a. Int -> (a -> a) -> Array a -> Maybe (Array a)
modifyAt1 i f array = modifyAt i f array

modifyAtIndices1 :: forall t a. Foldable t => t Int -> (a -> a) -> Array a -> Array a
modifyAtIndices1 t f array = modifyAtIndices t f array

alterAt1 :: forall a. Int -> (a -> Maybe a) -> Array a -> Maybe (Array a)
alterAt1 i f array = alterAt i f array

reverse1 :: forall a. Array a -> Array a
reverse1 array = reverse array

concatinate :: forall a. Array (Array a) -> Array a
concatinate array = concat array

concatMap1 :: forall a b. (a -> Array b) -> Array a -> Array b
concatMap1 array1 array2 = concatMap array1 array2

filter1 :: forall a. (a -> Boolean) -> Array a -> Array a
filter1 b array = filter b array

partition1 :: forall a. (a -> Boolean) -> Array a -> { yes :: Array a, no :: Array a }
partition1 b array = partition b array

filterA1 :: forall a f. Applicative f => (a -> f Boolean) -> Array a -> f (Array a)
filterA1 b array = filterA b array

mapMaybe1 :: forall a b. (a -> Maybe b) -> Array a -> Array b
mapMaybe1 m array = mapMaybe m array

catMaybes1 :: forall a. Array (Maybe a) -> Array a
catMaybes1 array = catMaybes array

mapWithIndex1 :: forall a b. (Int -> a -> b) -> Array a -> Array b
mapWithIndex1 f array = mapWithIndex f array

sort1 :: forall a. Ord a => Array a -> Array a
sort1 array = sort array

sortBy1 :: forall a. (a -> a -> Ordering) -> Array a -> Array a
sortBy1 a array = sortBy a array

sortWith1 :: forall a b. Ord b => (a -> b) -> Array a -> Array a
sortWith1 f array = sortWith f array

slice1 :: forall a. Int -> Int -> Array a -> Array a
slice1 i1 i2 array = slice i1 i2 array

take1 :: forall a. Int -> Array a -> Array a
take1 i array = take i array

takeEnd1 :: forall a. Int -> Array a -> Array a
takeEnd1 i array = takeEnd i array

drop1 :: forall a. Int -> Array a -> Array a
drop1 i array = drop i array

dropEnd1 :: forall a. Int -> Array a -> Array a
dropEnd1 i array = dropEnd i array

dropWhile1 :: forall a. (a -> Boolean) -> Array a -> Array a
dropWhile1 b array = dropWhile b array

span1 :: forall a. (a -> Boolean) -> Array a -> { init :: Array a, rest :: Array a }
span1 b array = span b array

group1 :: forall a. Eq a => Array a -> Array (NonEmptyArray a)
group1 array = group array

groupBy1 :: forall a. (a -> a -> Boolean) -> Array a -> Array (NonEmptyArray a)
groupBy1 b array = groupBy b array

nub1 :: forall a. Ord a => Array a -> Array a
nub1 array = nub array

nubEq1 :: forall a. Eq a => Array a -> Array a
nubEq1 array = nubEq array

nubBy1 :: forall a. (a -> a -> Ordering) -> Array a -> Array a
nubBy1 o array = nubBy o array

nubByEq1 :: forall a. (a -> a -> Boolean) -> Array a -> Array a
nubByEq1 b array = nubByEq b array

union1 :: forall a. Eq a => Array a -> Array a -> Array a
union1 array1 array2 = union array1 array2

unionBy1 :: forall a. (a -> a -> Boolean) -> Array a -> Array a -> Array a
unionBy1 b array1 array2 = unionBy b array1 array2

delete1 :: forall a. Eq a => a -> Array a -> Array a
delete1 a array = delete a array

deleteBy1 :: forall a. (a -> a -> Boolean) -> a -> Array a -> Array a
deleteBy1 b a array = deleteBy b a array

difference1 :: forall a. Eq a => Array a -> Array a -> Array a
difference1 array1 array2 = difference array1 array2

intersect1 :: forall a. Eq a => Array a -> Array a -> Array a
intersect1 array1 array2 = intersect array1 array2

intersectBy1 :: forall a. (a -> a -> Boolean) -> Array a -> Array a -> Array a
intersectBy1 b array1 array2 = intersectBy b array1 array2

zipWith1 :: forall a b c. (a -> b -> c) -> Array a -> Array b -> Array c
zipWith1 f array1 array2 = zipWith f array1 array2

zipWithA1 :: forall m a b c. Applicative m => (a -> b -> m c) -> Array a -> Array b -> m (Array c)
zipWithA1 f array1 array2 = zipWithA f array1 array2

zip1 :: forall a b. Array a -> Array b -> Array (Tuple a b)
zip1 array1 array2 = zip array1 array2

unzip1 :: forall a b. Array (Tuple a b) -> Tuple (Array a) (Array b)
unzip1 array = unzip array

foldM1 :: forall m a b. Monad m => (a -> b -> m a) -> a -> Array b -> m a
foldM1 f a array = foldM f a array

foldRecM1 :: forall m a b. MonadRec m => (a -> b -> m a) -> a -> Array b -> m a
foldRecM1 f a array = foldRecM f a array

unsafeIndex1 :: forall a. Partial => Array a -> Int -> a
unsafeIndex1 array i = unsafeIndex array i

main :: Effect Unit
main = do
  logShow $ fromFoldable1 $ Just array
  logShow $ toUnfoldable array :: List _
  logShow $ singleton1 "abc"
  logShow $ range1 90 4
  logShow $ replicate1 3 "abc"
  -- logShow $ some1 array1
  -- logShow $ many1 array
  logShow $ null1 array
  logShow $ length1 array
  logShow $ cons1 49.76 array
  logShow $ snoc1 array 56.093
  logShow $ insert1 5.54 array
  logShow $ insertBy1 invertCompare 4.9 array
  logShow $ head1 array
  logShow $ last1 array
  logShow $ tail1 array
  logShow $ init1 array
  logShow $ uncons1 array
  logShow $ unsnoc1 array
  logShow $ index1 array 4
  logShow $ elemIndex1 3.5 array
  logShow $ elemLastIndex1 3.5 array
  logShow $ findIndex1 (contains (Pattern "c")) arraystr
  logShow $ findLastIndex1 (contains (Pattern "a")) arraystr
  logShow $ insertAt1 3 3.5 array
  logShow $ deleteAt1 4 array
  logShow $ updateAt1 2 4.5 array
  logShow $ updateAtIndices1 [Tuple 1 "a"] arraystr
  logShow $ modifyAt1 4 toUpper arraystr
  logShow $ modifyAtIndices1 [1, 3] toUpper arraystr
  logShow $ alterAt1 8 maybeItself arrayint
  logShow $ reverse1 array
  logShow $ concatinate arrayar
  logShow $ concatMap1 singleton1 array
  logShow $ filter1 (contains (Pattern "b")) arraystr
  logShow $ partition1 (contains (Pattern "c")) arraystr
  logShow $ filterA1 maybeBoolean arrayint
  logShow $ mapMaybe1 fromNumber array
  logShow $ catMaybes1 arraym
  logShow $ mapWithIndex1 add2 arrayint
  logShow $ sortBy1 invertCompare array
  logShow $ sort1 array
  logShow $ sortWith1 add3 arrayint
  logShow $ slice1 1 4 array
  logShow $ take1 3 array
  logShow $ takeEnd1 3 array
  logShow $ drop1 2 array
  logShow $ dropEnd1 3 array
  logShow $ dropWhile1 (contains (Pattern "d")) arraystr
  logShow $ span1 (contains (Pattern "c")) arraystr
  logShow $ group1 array
  logShow $ groupBy1 greater arrayint
  logShow $ nub1 array
  logShow $ nubEq1 array
  logShow $ nubBy1 invertCompare array
  logShow $ nubByEq1 greater arrayint
  logShow $ union1 array array2
  logShow $ unionBy1 greater arrayint arrayin
  logShow $ delete1 3.5 array
  logShow $ deleteBy1 greater 5 arrayint
  logShow $ difference1 array array2
  logShow $ intersect1 array array2
  logShow $ intersectBy1 greater arrayint arrayin
  logShow $ zipWith1 add array array2
  logShow $ zipWithA1 add1 arrayin arrayint
  logShow $ zip1 arraystr array
  logShow $ unzip1 arraytuple
  logShow $ foldM1 add1 5 arrayint
  logShow $ foldRecM1 add1 4 arrayint
  logShow $ unsafePartial $ unsafeIndex1 arrayint 4
  logShow $ "------------ Operators -------------"
  logShow $ 4.37 : array -- Operator alias for Data.Array.cons (right-associative / precedence 6)
  logShow $ array !! 2 -- Operator alias for Data.Array.index (left-associative / precedence 8)
  logShow $ array \\ array2 -- Operator alias for Data.Array.difference (non-associative / precedence 5)
  logShow $ 3..19 -- Operator alias for Data.Array.NonEmpty.range (non-associative / precedence 8)
