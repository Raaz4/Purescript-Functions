module Main where

import Control.Alternative
import Control.Lazy
import Control.Monad.Rec.Class
import Data.Foldable
import Data.Int
import Data.List
import Data.List.Types
import Data.Maybe
import Data.Ordering
import Data.Unfoldable
import Prelude
import Data.Tuple
import Data.List (group, null, foldM, singleton, range, length)
import Effect (Effect)
import Effect.Class.Console (logShow)

add :: Int -> Int
add b = 3 + b

addi :: Int -> Int -> Int
addi a b = a + b

add1 :: Number -> Number -> Number
add1 a b = a + b

add4 :: Int -> Int -> Maybe Int
add4 i i1 = Just (i+i1)

itself :: Int -> Int
itself a = a

maybeItself :: Int -> Maybe Int
maybeItself a = (Just a)

greater :: Int -> Int -> Boolean
greater i i1 = if i > i1 then true else false

listm :: List (Maybe Int)
listm = ((Just 2):(Just 3):(Just 124):(Just 545):(Just 32):(Just 22):(Just 24):(Just 32):Nil)

invertCompare a b = invert $ compare a b

maybeBoolean :: Int -> Maybe Boolean
maybeBoolean i = if i `mod` 2 == 0 then (Just true) else (Just false)

array :: Array (List Int)
array = [(1:3:2:3:Nil),(2:6:32:2:342:Nil),(4:34:435:4235:25:Nil)]

list :: List Int
list = (6:6:3:3:7:7:2:2:5:5:Nil)

liststr :: List String
liststr = ("a":"b":"c":"d":"e":Nil)

listTuples :: List (Tuple String Int)
listTuples = ((Tuple "a" 1):(Tuple "b" 2):(Tuple "c" 3):(Tuple "d" 4):(Tuple "e" 5):(Tuple "f" 6):Nil)

listi :: List Int
listi = (4:4:6:7:546:43:Nil)

listn :: List Number
listn = (2.00:2.3:34.44:4.3:343.3:3.3:Nil)

listl :: List (List Int)
listl = ((3:2:4:4:45:3:Nil):(2:34:45:45:3:45:43:Nil):(1:2:3:32:34:45:54:Nil):Nil)

toUnfoldable1 :: forall f. Unfoldable f => List ~> f
toUnfoldable1 list = toUnfoldable list

fromFoldable1 :: forall f. Foldable f => f ~> List
fromFoldable1 f = fromFoldable f

singleton1 :: forall a. a -> List a
singleton1 a = singleton a

range1 :: Int -> Int -> List Int
range1 i ii = range i ii

some1 :: forall f a. Alternative f => Lazy (f (List a)) => f a -> f (List a)
some1 f = some f

someRec1 :: forall f a. MonadRec f => Alternative f => f a -> f (List a)
someRec1 f = someRec f

many1 :: forall f a. Alternative f => Lazy (f (List a)) => f a -> f (List a)
many1 f = many f

manyRec1 :: forall f a. MonadRec f => Alternative f => f a -> f (List a)
manyRec1 f = manyRec f

null1 :: forall a. List a -> Boolean
null1 l = null l

length1 :: forall a. List a -> Int
length1 list =length list

snoc1 :: forall a. List a -> a -> List a
snoc1 a list = snoc a list

insert1 :: forall a. Ord a => a -> List a -> List a
insert1 a l = insert a l

insertBy1 :: forall a. (a -> a -> Ordering) -> a -> List a -> List a
insertBy1 o a l = insertBy o a l

head1 :: List Int -> Maybe Int
head1 list = head list

last1 :: List Int -> Maybe Int
last1 list = last list

tail1 :: forall a. List a -> Maybe (List a)
tail1 list = tail list

init1 :: forall a. List a -> Maybe (List a)
init1 list = init list

uncons1 :: forall a. List a -> Maybe { head :: a, tail :: List a }
uncons1 list = uncons list

unsnoc1 :: forall a. List a -> Maybe { init :: List a, last :: a }
unsnoc1 list = unsnoc list

index1 :: forall a. List a -> Int -> Maybe a
index1 list i = index list i

elemIndex1 :: forall a. Eq a => a -> List a -> Maybe Int
elemIndex1 a l = elemIndex a l

elemLastIndex1 :: forall a. Eq a => a -> List a -> Maybe Int
elemLastIndex1 a list = elemLastIndex a list

findIndex1 :: forall a. (a -> Boolean) -> List a -> Maybe Int
findIndex1 b list = findIndex b list

findLastIndex1 :: forall a. (a -> Boolean) -> List a -> Maybe Int
findLastIndex1 b list = findLastIndex b list

insertAt1 :: forall a. Int -> a -> List a -> Maybe (List a)
insertAt1 i b list = insertAt i b list

deleteAt1 :: forall a. Int -> List a -> Maybe (List a)
deleteAt1 i list = deleteAt i list

updateAt1 :: forall a. Int -> a -> List a -> Maybe (List a)
updateAt1 i a list = updateAt i a list

modifyAt1 :: forall a. Int -> (a -> a) -> List a -> Maybe (List a)
modifyAt1 i a list = modifyAt i a list

alterAt1 :: forall a. Int -> (a -> Maybe a) -> List a -> Maybe (List a)
alterAt1 i m list = alterAt i m list

reverse1 :: List ~> List
reverse1 list = reverse list

concat1 :: forall a. List (List a) -> List a
concat1 list = concat list

concatMap1 :: forall a b. (a -> List b) -> List a -> List b
concatMap1 a list = concatMap a list

filter1 :: forall a. (a -> Boolean) -> List a -> List a
filter1 b list = filter b list

filterM1 :: forall a m. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM1 b list = filterM b list

mapMaybe1 :: forall a b. (a -> Maybe b) -> List a -> List b
mapMaybe1 m list = mapMaybe m list

catMaybes1 :: forall a. List (Maybe a) -> List a
catMaybes1 listm = catMaybes listm

mapWithIndex1 :: forall a b. (Int -> a -> b) -> List a -> List b
mapWithIndex1 add list = mapWithIndex add list

sort1 :: forall a. Ord a => List a -> List a
sort1 list = sort list

sortBy1 :: forall a. (a -> a -> Ordering) -> List a -> List a
sortBy1 o list = sortBy o list

stripPrefix1 :: forall a. Eq a => Pattern a -> List a -> Maybe (List a)
stripPrefix1 p list = stripPrefix p list

slice1 :: Int -> Int -> List Int -> List Int
slice1 i ii list = slice i ii list

take1 :: forall a. Int -> List a -> List a
take1 i list = take i list

takeEnd1 :: forall a. Int -> List a -> List a
takeEnd1 i list = takeEnd i list

takeWhile1 :: forall a. (a -> Boolean) -> List a -> List a
takeWhile1 b list = takeWhile b list

dropEnd1 :: forall a. Int -> List a -> List a
dropEnd1 i list = dropEnd i list

span1 :: forall a. (a -> Boolean) -> List a -> { init :: List a, rest :: List a }
span1 b list = span b list

group1 :: forall a. Eq a => List a -> List (NonEmptyList a)
group1 list = group list

group'1 :: forall a. Ord a => List a -> List (NonEmptyList a)
group'1 list = group' list

groupBy1 :: forall a. (a -> a -> Boolean) -> List a -> List (NonEmptyList a)
groupBy1 b list = groupBy b list

partition1 :: forall a. (a -> Boolean) -> List a -> { yes :: List a, no :: List a }
partition1 b list = partition b list

nub1 :: forall a. Eq a => List a -> List a
nub1 list = nub list

nubBy1 :: forall a. (a -> a -> Boolean) -> List a -> List a
nubBy1 b list = nubBy b list

union1 :: forall a. Eq a => List a -> List a -> List a
union1 list listn = union list listn

unionBy1 :: forall a. (a -> a -> Boolean) -> List a -> List a -> List a
unionBy1 b list1 list2 = unionBy b list1 list2

delete1 :: forall a. Eq a => a -> List a -> List a
delete1 a list = delete a list

deleteBy1 :: forall a. (a -> a -> Boolean) -> a -> List a -> List a
deleteBy1 b a list = deleteBy b a list

difference1 :: forall a. Eq a => List a -> List a -> List a
difference1 list listi = difference list listi

intersect1 :: forall a. Eq a => List a -> List a -> List a
intersect1 list listi = intersect list listi

intersectBy1 :: forall a. (a -> a -> Boolean) -> List a -> List a -> List a
intersectBy1 b list1 list2 = intersectBy b list1 list2

zipWith1 :: forall a b c. (a -> b -> c) -> List a -> List b -> List c
zipWith1 f list1 list2 = zipWith f list1 list2

zipWithA1 :: forall m a b c. Applicative m => (a -> b -> m c) -> List a -> List b -> m (List c)
zipWithA1 f list1 list2 = zipWithA f list1 list2

zip1 :: forall a b. List a -> List b -> List (Tuple a b)
zip1 list1 list2 = zip list1 list2

unzip1 :: forall a b. List (Tuple a b) -> Tuple (List a) (List b)
unzip1 list = unzip list

transpose1 :: forall a. List (List a) -> List (List a)
transpose1 list = transpose list

foldM1 :: forall m a b. Monad m => (a -> b -> m a) -> a -> List b -> m a
foldM1 m a list = foldM m a list

main :: Effect Unit
main = do
  logShow $ toUnfoldable1 list :: List _
  logShow $ fromFoldable1 list
  logShow $ singleton1 4
  logShow $ range1 3 8
  -- logShow $ some1 listlist
  -- logShow $ someRec1 (Just 4)
  -- logShow $ many1 array
  -- logShow $ manyRec1 array
  logShow $ null $ Nil
  logShow $ length1 list
  logShow $ snoc1 list 5
  logShow $ insert1 9 list
  logShow $ insertBy1 invertCompare 5 list
  logShow $ head1 list
  logShow $ last1 list
  logShow $ tail1 list
  logShow $ init1 list
  logShow $ uncons1 list
  logShow $ unsnoc1 list
  logShow $ index1 list 3
  logShow $ elemIndex1 5 list
  logShow $ elemLastIndex1 5 list
  logShow $ findIndex1 even list
  logShow $ findLastIndex1 odd list
  logShow $ insertAt1 2 3 list
  logShow $ deleteAt1 4 list
  logShow $ updateAt1 2 3 list
  logShow $ modifyAt1 3 itself list
  logShow $ alterAt1 4 maybeItself list
  logShow $ reverse1 list
  logShow $ concat1 listl
  logShow $ concatMap1 singleton list
  logShow $ filter1 odd list
  logShow $ filterM1 maybeBoolean list
  logShow $ mapMaybe1 fromNumber listn
  logShow $ catMaybes1 listm
  logShow $ mapWithIndex1 addi list
  logShow $ sort1 list
  logShow $ sortBy1 invertCompare list
  logShow $ stripPrefix1 (Pattern (6:Nil)) list
  logShow $ slice1 3 8 list
  logShow $ take1 3 list
  logShow $ takeEnd1 4 list
  logShow $ takeWhile1 odd list
  logShow $ dropEnd1 5 list
  logShow $ span1 odd list
  logShow $ group1 list
  logShow $ group'1 list
  logShow $ groupBy1 greater list
  logShow $ partition1 even list
  logShow $ nub1 list
  logShow $ nubBy1 greater list
  logShow $ union1 list listi
  logShow $ unionBy1 greater list listi
  logShow $ delete1 4 list
  logShow $ deleteBy1 greater 5 list
  logShow $ difference1 list listi
  logShow $ intersect1 list listi
  logShow $ intersectBy1 greater list listi
  logShow $ zipWith1 addi list listi
  logShow $ zipWithA1 add4 list listi
  logShow $ zip1 liststr listi
  logShow $ unzip1 listTuples
  logShow $ transpose1 listl
  logShow $ foldM1 add4 5 listi
  logShow $ "------------ Operators -------------"
  logShow $ 3..30 -- Operator alias for Data.List.range (non-associative / precedence 8)
  logShow $ list !! 5 -- Operator alias for Data.List.index (left-associative / precedence 8)
  logShow $ list \\ listi -- Operator alias for Data.List.difference (non-associative / precedence 5)
