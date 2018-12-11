module Main where

import Prelude
import Effect (Effect)
import Data.Int
import Data.Maybe

round1 :: Number -> Int
round1 r = round r

even1 :: Int -> Boolean
even1 e = even e

odd1 :: Int -> Boolean
odd1 o = odd o

fromNumber1 :: Number -> Maybe Int
fromNumber1 f = fromNumber f

fromString1 :: String -> Maybe Int
fromString1 s = fromString s

toNumber1 :: Int -> Number
toNumber1 t = toNumber t

quot1 :: Int -> Int -> Int
quot1 q d = quot q d

rem1 :: Int -> Int -> Int
rem1 q d = rem q d

pow1 :: Int -> Int -> Int
pow1 p r = pow p r

main :: Effect _
main = do
       logShow $ round1 6.0
       logShow $ even1 5
       logShow $ odd1 4
       logShow $ fromNumber1 11.0
       logShow $ fromString1 "11"
       logShow $ toNumber1 9
       logShow $ quot1 2 3
       logShow $ rem1 1 3
       logShow $ pow1 2 2
