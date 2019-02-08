module Main where

import Prelude
import Effect (Effect)
import Data.Maybe (Maybe(..))
import Data.String.CodePoints (CodePoint(..), codePointFromChar, singleton, toCodePointArray, fromCodePointArray, codePointAt, uncons, countPrefix, takeWhile, dropWhile)
import Effect.Class.Console (logShow)

codePointToBoolean :: CodePoint -> Boolean
codePointToBoolean c = if c == (codePointFromChar1 'r') then false else true

codePointFromChar1 :: Char -> CodePoint
codePointFromChar1 c = codePointFromChar c

singleton1 :: CodePoint -> String
singleton1 c = singleton c

fromCodePointArray1 :: Array CodePoint -> String
fromCodePointArray1 a = fromCodePointArray a

toCodePointArray1 :: String -> Array CodePoint
toCodePointArray1 s = toCodePointArray s

codePointAt1 :: Int -> String -> Maybe CodePoint
codePointAt1 i s = codePointAt i s

uncons1 :: String -> Maybe { head :: CodePoint, tail :: String }
uncons1 s = uncons s

countPrefix1 :: (CodePoint -> Boolean) -> String -> Int
countPrefix1 b s = countPrefix b s

takeWhile1 :: (CodePoint -> Boolean) -> String -> String
takeWhile1 b s = takeWhile b s

dropWhile1 :: (CodePoint -> Boolean) -> String -> String
dropWhile1 b s = dropWhile b s

main :: Effect Unit
main = do
  logShow $ codePointFromChar1 '%'
  logShow $ singleton1 (codePointFromChar1 'f')
  logShow $ fromCodePointArray1 (toCodePointArray "fromCodePointArray")
  logShow $ toCodePointArray1 "toCodePointArray"
  logShow $ codePointAt1 3 "codePointAt"
  logShow $ uncons1 "uncons"
  -- logShow $ countPrefix1 (\c -> codePointToInt c == 0x1D400) "𝐀𝐀 b c 𝐀"
  -- logShow $ takeWhile1
  -- logShow $ dropWhile1
