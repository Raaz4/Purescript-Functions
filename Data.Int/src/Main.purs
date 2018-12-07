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

