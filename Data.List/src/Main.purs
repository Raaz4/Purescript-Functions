module Main where

import Control.Alternative (class Alternative)
import Control.Lazy (class Lazy)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Foldable (class Foldable)
import Data.Int (even, fromNumber, odd)
import Data.List (Pattern(..), List(..), (:), alterAt, catMaybes, concat, concatMap, delete, deleteAt, deleteBy, difference, dropEnd, elemIndex, elemLastIndex, filter, filterM, findIndex, findLastIndex, foldM, fromFoldable, group, group', groupBy, head, index, init, insert, insertAt, insertBy, intersect, intersectBy, last, length, many, manyRec, mapMaybe, mapWithIndex, modifyAt, nub, nubBy, null, partition, range, reverse, singleton, slice, snoc, some, someRec, sort, sortBy, span, stripPrefix, tail, take, takeEnd, takeWhile, toUnfoldable, transpose, uncons, union, unionBy, unsnoc, unzip, updateAt, zip, zipWith, zipWithA, (!!), (..), (\\))
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Ordering (invert)
import Data.Unfoldable (class Unfoldable)
import Prelude (class Applicative, class Eq, class Monad, class Ord, type (~>), Ordering, Unit, compare, discard, mod, ($), (+), (==), (>))
import Data.Tuple (Tuple(..))
import Data.List (group, null, foldM, singleton, range, length)
import Effect (Effect)
import Effect.Class.Console (log, logShow)

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

invertCompare :: forall a. Ord a => a -> a -> Ordering
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

main :: Effect Unit
main = do
  log $ "toUnfoldable :: forall f. Unfoldable f => List ~> f"
  logShow $ toUnfoldable list :: List _
  log $ "fromFoldable :: forall f. Foldable f => f ~> List"
  logShow $ fromFoldable list
  log $ "singleton :: forall a. a -> List a"
  logShow $ singleton 4
  log $ "range :: Int -> Int -> List Int"
  logShow $ range 3 8
  -- log $ "some :: forall f a. Alternative f => Lazy (f (List a)) => f a -> f (List a)"
  -- logShow $ some list :: List (List Int)
  -- log $ "someRec :: forall f a. MonadRec f => Alternative f => f a -> f (List a)"
  -- logShow $ someRec (Just 4) :: Maybe (List Int)
  -- log $ "many :: forall f a. Alternative f => Lazy (f (List a)) => f a -> f (List a)"
  -- logShow $ many array
  -- log $ "manyRec :: forall f a. MonadRec f => Alternative f => f a -> f (List a)"
  -- logShow $ manyRec (Just 9)
  log $ "null :: forall a. List a -> Boolean"
  logShow $ null $ Nil
  log $ "length :: forall a. List a -> Int"
  logShow $ length list
  log $ "snoc :: forall a. List a -> a -> List a"
  logShow $ snoc list 5
  log $ "insert :: forall a. Ord a => a -> List a -> List a"
  logShow $ insert 9 list
  log $ "insertBy :: forall a. (a -> a -> Ordering) -> a -> List a -> List a"
  logShow $ insertBy invertCompare 5 list
  log $ "head :: List ~> Maybe"
  logShow $ head list
  log $ "last :: List ~> Maybe"
  logShow $ last list
  log $ "tail :: forall a. List a -> Maybe (List a)"
  logShow $ tail list
  log $ "init :: forall a. List a -> Maybe (List a)"
  logShow $ init list
  log $ "uncons :: forall a. List a -> Maybe { head :: a, tail :: List a }"
  logShow $ uncons list
  log $ "unsnoc :: forall a. List a -> Maybe { init :: List a, last :: a }"
  logShow $ unsnoc list
  log $ "index :: forall a. List a -> Int -> Maybe a"
  logShow $ index list 3
  log $ "elemIndex :: forall a. Eq a => a -> List a -> Maybe Int"
  logShow $ elemIndex 5 list
  log $ "elemLastIndex :: forall a. Eq a => a -> List a -> Maybe Int"
  logShow $ elemLastIndex 5 list
  log $ "findIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int"
  logShow $ findIndex even list
  log $ "findLastIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int"
  logShow $ findLastIndex odd list
  log $ "insertAt :: forall a. Int -> a -> List a -> Maybe (List a)"
  logShow $ insertAt 2 3 list
  log $ "deleteAt :: forall a. Int -> List a -> Maybe (List a)"
  logShow $ deleteAt 4 list
  log $ "updateAt :: forall a. Int -> a -> List a -> Maybe (List a)"
  logShow $ updateAt 2 3 list
  log $ "modifyAt :: forall a. Int -> (a -> a) -> List a -> Maybe (List a)"
  logShow $ modifyAt 3 itself list
  log $ "alterAt :: forall a. Int -> (a -> Maybe a) -> List a -> Maybe (List a)"
  logShow $ alterAt 4 maybeItself list
  log $ "reverse :: List ~> List"
  logShow $ reverse list
  log $ "concat :: forall a. List (List a) -> List a"
  logShow $ concat listl
  log $ "concatMap :: forall a b. (a -> List b) -> List a -> List b"
  logShow $ concatMap singleton list
  log $ "filter :: forall a. (a -> Boolean) -> List a -> List a"
  logShow $ filter odd list
  log $ "filterM :: forall a m. Monad m => (a -> m Boolean) -> List a -> m (List a)"
  logShow $ filterM maybeBoolean list
  log $ "mapMaybe :: forall a b. (a -> Maybe b) -> List a -> List b"
  logShow $ mapMaybe fromNumber listn
  log $ "catMaybes :: forall a. List (Maybe a) -> List a"
  logShow $ catMaybes listm
  log $ "mapWithIndex :: forall a b. (Int -> a -> b) -> List a -> List b"
  logShow $ mapWithIndex addi list
  log $ "sort :: forall a. Ord a => List a -> List a"
  logShow $ sort list
  log $ "sortBy :: forall a. (a -> a -> Ordering) -> List a -> List a"
  logShow $ sortBy invertCompare list
  log $ "stripPrefix :: forall a. Eq a => Pattern a -> List a -> Maybe (List a)"
  logShow $ stripPrefix (Pattern (6:Nil)) list
  log $ "slice :: Int -> Int -> List ~> List"
  logShow $ slice 3 8 list
  log $ "take :: forall a. Int -> List a -> List a"
  logShow $ take 3 list
  log $ "takeEnd :: forall a. Int -> List a -> List a"
  logShow $ takeEnd 4 list
  log $ "takeWhile :: forall a. (a -> Boolean) -> List a -> List a"
  logShow $ takeWhile odd list
  log $ "drop :: forall a. Int -> List a -> List a"
  logShow $ dropEnd 5 list
  log $ "dropEnd :: forall a. Int -> List a -> List a"
  logShow $ span odd list
  log $ "dropWhile :: forall a. (a -> Boolean) -> List a -> List a"
  logShow $ dropWhile odd list
  log $ "span :: forall a. (a -> Boolean) -> List a -> { init :: List a, rest :: List a }"
  logShow $ span odd list
  log $ "group :: forall a. Eq a => List a -> List (NonEmptyList a)"
  logShow $ group list
  log $ "group' :: forall a. Ord a => List a -> List (NonEmptyList a)"
  logShow $ group' list
  log $ "groupBy :: forall a. (a -> a -> Boolean) -> List a -> List (NonEmptyList a)"
  logShow $ groupBy greater list
  log $ "partition :: forall a. (a -> Boolean) -> List a -> { yes :: List a, no :: List a }"
  logShow $ partition even list
  log $ "nub :: forall a. Eq a => List a -> List a"
  logShow $ nub list
  log $ "nubBy :: forall a. (a -> a -> Boolean) -> List a -> List a"
  logShow $ nubBy greater list
  log $ "union :: forall a. Eq a => List a -> List a -> List a"
  logShow $ union list listi
  log $ "unionBy :: forall a. (a -> a -> Boolean) -> List a -> List a -> List a"
  logShow $ unionBy greater list listi
  log $ "delete :: forall a. Eq a => a -> List a -> List a"
  logShow $ delete 4 list
  log $ "deleteBy :: forall a. (a -> a -> Boolean) -> a -> List a -> List a"
  logShow $ deleteBy greater 5 list
  log $ "difference :: forall a. Eq a => List a -> List a -> List a"
  logShow $ difference list listi
  log $ "intersect :: forall a. Eq a => List a -> List a -> List a"
  logShow $ intersect list listi
  log $ "intersectBy :: forall a. (a -> a -> Boolean) -> List a -> List a -> List a"
  logShow $ intersectBy greater list listi
  log $ "zipWith :: forall a b c. (a -> b -> c) -> List a -> List b -> List c"
  logShow $ zipWith addi list listi
  log $ "zipWithA :: forall m a b c. Applicative m => (a -> b -> m c) -> List a -> List b -> m (List c)"
  logShow $ zipWithA add4 list listi
  log $ "zip :: forall a b. List a -> List b -> List (Tuple a b)"
  logShow $ zip liststr listi
  log $ "unzip :: forall a b. List (Tuple a b) -> Tuple (List a) (List b)"
  logShow $ unzip listTuples
  log $ "transpose :: forall a. List (List a) -> List (List a)"
  logShow $ transpose listl
  log $ "foldM :: forall m a b. Monad m => (a -> b -> m a) -> a -> List b -> m a"
  logShow $ foldM add4 5 listi
  logShow $ "------------ Operators -------------"
  logShow $ 3..30 -- Operator alias for Data.List.range (non-associative / precedence 8)
  logShow $ list !! 5 -- Operator alias for Data.List.index (left-associative / precedence 8)
  logShow $ list \\ listi -- Operator alias for Data.List.difference (non-associative / precedence 5)
