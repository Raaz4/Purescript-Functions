module Main where

import Data.String.CodePoints (CodePoint, codePointAt, codePointFromChar, fromCodePointArray, singleton, toCodePointArray, uncons)
import Effect (Effect)
import Data.StrMap
import Effect.Class.Console (logShow, log)
import Prelude

codePointToBoolean :: CodePoint -> Boolean
codePointToBoolean c = if c == (codePointFromChar 'r') then false else true

main :: Effect Unit
main = do
  log $ "codePointFromChar :: Char -> CodePoint"
  logShow $ codePointFromChar '%'
  log $ "singleton :: CodePoint -> String"
  logShow $ singleton (codePointFromChar 'f')
  log $ "fromCodePointArray :: Array CodePoint -> String"
  logShow $ fromCodePointArray (toCodePointArray "fromCodePointArray")
  log $ "toCodePointArray :: String -> Array CodePoint"
  logShow $ toCodePointArray "toCodePointArray"
  log $ "codePointAt :: Int -> String -> Maybe CodePoint"
  logShow $ codePointAt 3 "codePointAt"
  log $ "uncons :: String -> Maybe { head :: CodePoint, tail :: String }"
  logShow $ uncons "uncons"
  log $ "countPrefix :: (CodePoint -> Boolean) -> String -> Int"
  -- logShow $ countPrefix (\c -> codePointToInt c == 0x1D400) "𝐀𝐀 b c 𝐀"
  log $ "takeWhile :: (CodePoint -> Boolean) -> String -> String"
  -- logShow $ takeWhile
  log $ "dropWhile :: (CodePoint -> Boolean) -> String -> String"
  log $ insert "asasjh" "asdfsadf" empty
  -- logShow $ dropWhile
