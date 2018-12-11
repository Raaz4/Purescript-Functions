module Main where

import Prelude
import Effect (Effect)
import Effect.Class.Console (logShow)
import Data.List (group)
import Data.List
import Data.Int
import Data.List.Types
import Data.Maybe
import Data.Ordering

add b = 3 + b

add1 :: Number -> Number -> Number
add1 a b = a + b

toNum a b = fromNumber $ add1 a b

listm = ((Just 2):(Just 3):(Just 124):(Just 545):(Just 32):(Just 22):(Just 24):(Just 32):Nil)

invertCompare a b = invert $ compare a b

list :: List Int
list = (6:6:3:3:7:7:2:2:5:5:Nil)

listi = (4:4:6:7:546:43:Nil)

listn = (2.00:2.3:34.44:4.3:343.3:3.3:Nil)

listl = ((3:2:4:4:45:3:Nil):(2:34:45:45:3:45:43:Nil):(1:2:3:32:34:45:54:Nil):Nil)

null1 :: forall a. List a -> Boolean
null1 l = null l

insertBy1 :: forall a. (a -> a -> Ordering) -> a -> List a -> List a
insertBy1 o a l = insertBy o a l

insert1 :: forall a. Ord a => a -> List a -> List a
insert1 a l = insert a l

elemIndex1 :: forall a. Eq a => a -> List a -> Maybe Int
elemIndex1 a l = elemIndex a l

elemLastIndex1 :: forall a. Eq a => a -> List a -> Maybe Int
elemLastIndex1 a list = elemLastIndex a list

findIndex1 :: forall a. (a -> Boolean) -> List a -> Maybe Int
findIndex1 b list = findIndex b list

insertAt1 :: forall a. Int -> a -> List a -> Maybe (List a)
insertAt1 i b list = insertAt i b list

deleteAt1 :: forall a. Int -> List a -> Maybe (List a)
deleteAt1 i list = deleteAt i list

updateAt1 :: forall a. Int -> a -> List a -> Maybe (List a)
updateAt1 i a list = updateAt i a list

reverse1 :: List ~> List
reverse1 list = reverse list

filter1 :: forall a. (a -> Boolean) -> List a -> List a
filter1 b list = filter b list

mapMaybe1 :: forall a b. (a -> Maybe b) -> List a -> List b
mapMaybe1 m list = mapMaybe m list

catMaybes1 :: forall a. List (Maybe a) -> List a
catMaybes1 listm = catMaybes listm

mapWithIndex1 :: forall a b. (Int -> a -> b) -> List a -> List b
mapWithIndex1 add list = mapWithIndex add list

takeEnd1 :: forall a. Int -> List a -> List a
takeEnd1 i list = takeEnd i list

takeWhile1 :: forall a. (a -> Boolean) -> List a -> List a
takeWhile1 b list = takeWhile b list

span1 :: forall a. (a -> Boolean) -> List a -> { init :: List a, rest :: List a }
span1 b list = span b list

sort1 :: forall a. Ord a => List a -> List a
sort1 list = sort list

sortBy1 :: forall a. (a -> a -> Ordering) -> List a -> List a
sortBy1 o list = sortBy o list

group1 :: forall a. Eq a => List a -> List (NonEmptyList a)
group1 list = group list

partition1 :: forall a. (a -> Boolean) -> List a -> { yes :: List a, no :: List a }
partition1 b list = partition b list

union1 :: forall a. Eq a => List a -> List a -> List a
union1 list listn = union list listn

delete1 :: forall a. Eq a => a -> List a -> List a
delete1 a list = delete a list

difference1 :: forall a. Eq a => List a -> List a -> List a
difference1 list listi = difference list listi

intersect1 :: forall a. Eq a => List a -> List a -> List a
intersect1 list listi = intersect list listi

transpose1 :: forall a. List (List a) -> List (List a)
transpose1 list = transpose list

foldM1 :: forall m a b. Monad m => (a -> b -> m a) -> a -> List b -> m a
foldM1 m a list = foldM m a list

main :: Effect Unit
main = do
  logShow $ list
  logShow $ null $ Nil
  logShow $ insertBy1 invertCompare 5 list
  logShow $ insert1 9 list
  logShow $ elemIndex1 5 list
  logShow $ elemLastIndex1 5 list
  logShow $ findIndex1 even list
  logShow $ insertAt1 2 3 list
  logShow $ deleteAt1 4 list
  logShow $ updateAt1 2 3 list
  logShow $ reverse1 list
  logShow $ filter1 odd list
  logShow $ mapMaybe1 fromNumber listn
  logShow $ catMaybes1 listm
  -- logShow $ mapWithIndex1 drop list
  logShow $ takeEnd1 4 list
  logShow $ takeWhile1 odd list
  logShow $ span1 odd list
  logShow $ sort1 list
  logShow $ sortBy1 invertCompare list
  logShow $ group1 list
  logShow $ partition1 even list
  logShow $ union1 list listi
  logShow $ delete1 4 list
  logShow $ difference1 list listi
  logShow $ intersect1 list listi
  logShow $ transpose1 listl
  -- logShow $ foldM1 toNum 3 list
