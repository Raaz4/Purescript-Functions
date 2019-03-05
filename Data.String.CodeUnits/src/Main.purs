module Main where

import Prelude
import Effect (Effect)
import Effect.Class.Console (logShow, log)
import Data.String.CodeUnits (singleton, stripPrefix, stripSuffix, contains, fromCharArray, toCharArray, charAt, toChar, uncons, length, countPrefix, indexOf, indexOf', lastIndexOf, lastIndexOf', take, takeRight, takeWhile, drop, dropRight, dropWhile, slice, splitAt)
import Data.String (Pattern(..))

alphabets :: Char -> Boolean
alphabets c = if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') then true else false

main :: Effect Unit
main = do
    log $ "stripPrefix :: Pattern -> String -> Maybe String"
    logShow $ stripPrefix (Pattern "str") "stripPrefix"
    log $ "stripSuffix :: Pattern -> String -> Maybe String"
    logShow $ stripSuffix (Pattern "fix") "stripSuffix"
    log $ "contains :: Pattern -> String -> Boolean"
    logShow $ contains (Pattern "@") "cont@ins"
    log $ "singleton :: Char -> String"
    logShow $ singleton 'r'
    log $ "fromCharArray :: Array Char -> String"
    logShow $ fromCharArray ['f','r','o','m','C','h','a','r','A','r','r','a','y']
    log $ "toCharArray :: String -> Array Char"
    logShow $ toCharArray "toCharArray"
    log $ "charAt :: Int -> String -> Maybe Char"
    logShow $ charAt 4 "charAt"
    log $ "toChar :: String -> Maybe Char"
    logShow $ toChar "r"
    log $ "uncons :: String -> Maybe { head :: Char, tail :: String }"
    logShow $ uncons "uncons"
    log $ "length :: String -> Int"
    logShow $ length "length"
    log $ "countPrefix :: (Char -> Boolean) -> String -> Int"
    logShow $ countPrefix alphabets "abc@%D"
    log $ "indexOf :: Pattern -> String -> Maybe Int"
    logShow $ indexOf (Pattern "e") "indexOf"
    log $ "indexOf' :: Pattern -> Int -> String -> Maybe Int"
    logShow $ indexOf' (Pattern "e") 1 "indexOf'"
    log $ "lastIndexOf :: Pattern -> String -> Maybe Int"
    logShow $ lastIndexOf (Pattern "@") "l@stIndexOf"
    log $ "lastIndexOf' :: Pattern -> Int -> String -> Maybe Int"
    logShow $ lastIndexOf' (Pattern "@") 10 "l@stIndexOf'"
    log $ "take :: Int -> String -> String"
    logShow $ take 2 "take"
    log $ "takeRight :: Int -> String -> String"
    logShow $ takeRight 5 "takeRight"
    log $ "takeWhile :: (Char -> Boolean) -> String -> String"
    logShow $ takeWhile alphabets "t@keWhile"
    log $ "drop :: Int -> String -> String"
    logShow $ drop 2 "drop"
    log $ "dropRight :: Int -> String -> String"
    logShow $ dropRight 4 "dropRight"
    log $ "dropWhile :: (Char -> Boolean) -> String -> String"
    logShow $ dropWhile alphabets "drop#While"
    log $ "slice :: Int -> Int -> String -> Maybe String"
    logShow $ slice 1 4 "slice"
    log $ "splitAt :: Int -> String -> { before :: String, after :: String }"
    logShow $ splitAt 3 "splitAt"
