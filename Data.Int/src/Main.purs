module Main where

import Prelude
import Effect (Effect)
import Effect.Class.Console (logShow) 
import Data.Int
import Data.Maybe

fromNumber1 :: Number -> Maybe Int
fromNumber1 f = fromNumber f

ceil1 :: Number -> Int
ceil1 n = ceil n

floor1 :: Number -> Int
floor1 n = floor n

round1 :: Number -> Int
round1 r = round r

toNumber1 :: Int -> Number
toNumber1 t = toNumber t

fromString1 :: String -> Maybe Int
fromString1 s = fromString s

radix1 :: Int -> Maybe Radix
radix1 i = radix i

binary1 :: Radix
binary1 = binary

octal1 :: Radix
octal1 = octal

decimal1 :: Radix
decimal1 = decimal

hexadecimal1 :: Radix
hexadecimal1 = hexadecimal

base361 :: Radix
base361 = base36

fromStringAs1 :: Radix -> String -> Maybe Int
fromStringAs1 r s = fromStringAs r s

toStringAs1 :: Radix -> Int -> String
toStringAs1 r i = toStringAs r i

parity1 :: Int -> Parity
parity1 i = parity i

even1 :: Int -> Boolean
even1 e = even e

odd1 :: Int -> Boolean
odd1 o = odd o

quot1 :: Int -> Int -> Int
quot1 q d = quot q d

rem1 :: Int -> Int -> Int
rem1 q d = rem q d

pow1 :: Int -> Int -> Int
pow1 p r = pow p r

main :: Effect _
main = do
       logShow $ fromNumber1 11.0
       logShow $ ceil1 9.332
       logShow $ floor1 3.3213
       logShow $ round1 6.0
       logShow $ toNumber1 9
       logShow $ fromString1 "11"
    --    logShow $ radix1 8
       logShow $ fromStringAs1 binary "1000010101"
       logShow $ toStringAs1 binary 1012
       logShow $ parity1 8
       logShow $ even1 5
       logShow $ odd1 4
       logShow $ quot1 2 3
       logShow $ rem1 1 3
       logShow $ pow1 2 2
