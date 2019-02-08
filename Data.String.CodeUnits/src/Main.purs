module Main where

import Prelude
import Effect (Effect)
import Effect.Class.Console (logShow)
import Data.String.CodeUnits (singleton, stripPrefix, stripSuffix, contains, fromCharArray, toCharArray, charAt, toChar, uncons, length, countPrefix, indexOf, indexOf', lastIndexOf, lastIndexOf', take, takeRight, takeWhile, drop, dropRight, dropWhile, slice, splitAt)
import Data.String (Pattern(..))
import Data.Maybe (Maybe)

alphabets :: Char -> Boolean
alphabets c = if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') then true else false

stripPrefix1 :: Pattern -> String -> Maybe String
stripPrefix1 p s = stripPrefix p s

stripSuffix1 :: Pattern -> String -> Maybe String
stripSuffix1 p s = stripSuffix p s

contains1 :: Pattern -> String -> Boolean
contains1 p s = contains p s

singleton1 :: Char -> String
singleton1 c = singleton c

fromCharArray1 :: Array Char -> String
fromCharArray1 a = fromCharArray a

toCharArray1 :: String -> Array Char
toCharArray1 s = toCharArray s

charAt1 :: Int -> String -> Maybe Char
charAt1 i s = charAt i s

toChar1 :: String -> Maybe Char
toChar1 s = toChar s

uncons1 :: String -> Maybe { head :: Char, tail :: String }
uncons1 s = uncons s

length1 :: String -> Int
length1 s = length s

countPrefix1 :: (Char -> Boolean) -> String -> Int
countPrefix1 b s = countPrefix b s

indexOf1 :: Pattern -> String -> Maybe Int
indexOf1 p s = indexOf p s

indexOf'1 :: Pattern -> Int -> String -> Maybe Int
indexOf'1 p i s = indexOf' p i s

lastIndexOf1 :: Pattern -> String -> Maybe Int
lastIndexOf1 p s = lastIndexOf p s

lastIndexOf'1 :: Pattern -> Int -> String -> Maybe Int
lastIndexOf'1 p i s = lastIndexOf' p i s

take1 :: Int -> String -> String
take1 i s = take i s

takeRight1 :: Int -> String -> String
takeRight1 i s = takeRight i s

takeWhile1 :: (Char -> Boolean) -> String -> String
takeWhile1 b s = takeWhile b s

drop1 :: Int -> String -> String
drop1 i s = drop i s

dropRight1 :: Int -> String -> String
dropRight1 i s = dropRight i s

dropWhile1 :: (Char -> Boolean) -> String -> String
dropWhile1 b s = dropWhile b s

slice1 :: Int -> Int -> String -> Maybe String
slice1 i1 i2 s = slice i1 i2 s

splitAt1 :: Int -> String -> { before :: String, after :: String }
splitAt1 i s = splitAt i s

main :: Effect Unit
main = do
  logShow $ stripPrefix1 (Pattern "str") "stripPrefix"
  logShow $ stripSuffix1 (Pattern "fix") "stripSuffix"
  logShow $ contains1 (Pattern "@") "cont@ins"
  logShow $ singleton1 'r'
  logShow $ fromCharArray1 ['f','r','o','m','C','h','a','r','A','r','r','a','y']
  logShow $ toCharArray1 "toCharArray"
  logShow $ charAt1 4 "charAt"
  logShow $ toChar1 "r"
  logShow $ uncons1 "uncons"
  logShow $ length1 "length"
  logShow $ countPrefix1 alphabets "abc@%D"
  logShow $ indexOf1 (Pattern "e") "indexOf"
  logShow $ indexOf'1 (Pattern "e") 1 "indexOf'"
  logShow $ lastIndexOf1 (Pattern "@") "l@stIndexOf"
  logShow $ lastIndexOf'1 (Pattern "@") 10 "l@stIndexOf'"
  logShow $ take1 2 "take"
  logShow $ takeRight1 5 "takeRight"
  logShow $ takeWhile1 alphabets "t@keWhile"
  logShow $ drop1 2 "drop"
  logShow $ dropRight1 4 "dropRight"
  logShow $ dropWhile1 alphabets "drop#While"
  logShow $ slice1 1 4 "slice"
  logShow $ splitAt1 3 "splitAt"
