module Main where

import Data.Int (Parity, Radix, base36, binary, ceil, decimal, even, floor, fromNumber, fromString, fromStringAs, hexadecimal, octal, odd, parity, pow, quot, radix, rem, round, toNumber, toStringAs)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Class.Console (log, logShow, infoShow) 
import Prelude (discard, ($), Unit)
import Test.Assert (assert)

main :: Effect Unit
main = do
        log $ "base36 :: Radix"
        log $ "binary :: Radix"
        log $ "ceil :: Number -> Int"
        logShow $ ceil 9.332
        log $ "decimal :: Radix"
        log $ "even :: Int -> Boolean"
        logShow $ even 5
        log $ "floor :: Number -> Int"
        logShow $ floor 3.3213
        log $ "fromNumber :: Number -> Maybe Int"
        logShow $ fromNumber 11.0
        log $ "fromString :: String -> Maybe Int"
        logShow $ fromString "11"
        log $ "fromStringAs :: Radix -> String -> Maybe Int"
        logShow $ fromStringAs binary "1000010101"
        log $ "hexadecimal :: Radix"
        log $ "octal :: Radix"
        log $ "odd :: Int -> Boolean"
        logShow $ odd 4
        log $ "parity :: Int -> Parity"
        logShow $ parity 8
        log $ "pow :: Int -> Int -> Int"
        logShow $ pow 2 2
        log $ "quot :: Int -> Int -> Int"
        logShow $ quot 2 3
        log $ "radix :: Int -> Maybe Radix"
        log $ "rem :: Int -> Int -> Int"
        logShow $ rem 1 3
        log $ "round :: Number -> Int"
        logShow $ round 6.0
        log $ "toNumber :: Int -> Number"
        logShow $ toNumber 9
        log $ "toStringAs :: Radix -> Int -> String"
        logShow $ toStringAs binary 1012
        
