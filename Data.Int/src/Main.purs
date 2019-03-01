module Main where

import Prelude (discard, ($), Unit)
import Effect (Effect)
import Test.Assert (assert)
import Effect.Class.Console (log, logShow, infoShow) 
import Data.Int (Parity, Radix, base36, binary, ceil, decimal, even, floor, fromNumber, fromString, fromStringAs, hexadecimal, octal, odd, parity, pow, quot, radix, rem, round, toNumber, toStringAs)
import Data.Maybe (Maybe)

main :: Effect Unit
main = do
        log $ "fromNumber :: Number -> Maybe Int"
        logShow $ fromNumber 11.0
        log $ "ceil :: Number -> Int"
        logShow $ ceil 9.332
        log $ "floor :: Number -> Int"
        logShow $ floor 3.3213
        log $ "round :: Number -> Int"
        logShow $ round 6.0
        log $ "toNumber :: Int -> Number"
        logShow $ toNumber 9
        log $ "fromString :: String -> Maybe Int"
        logShow $ fromString "11"
        log $ "radix :: Int -> Maybe Radix"
        log $ "binary :: Radix"
        log $ "octal :: Radix"
        log $ "decimal :: Radix"
        log $ "hexadecimal :: Radix"
        log $ "base36 :: Radix"
        log $ "fromStringAs :: Radix -> String -> Maybe Int"
        logShow $ fromStringAs binary "1000010101"
        log $ "toStringAs :: Radix -> Int -> String"
        logShow $ toStringAs binary 1012
        log $ "parity :: Int -> Parity"
        logShow $ parity 8
        log $ "even :: Int -> Boolean"
        logShow $ even 5
        log $ "odd :: Int -> Boolean"
        logShow $ odd 4
        log $ "quot :: Int -> Int -> Int"
        logShow $ quot 2 3
        log $ "rem :: Int -> Int -> Int"
        logShow $ rem 1 3
        log $ "pow :: Int -> Int -> Int"
        logShow $ pow 2 2
