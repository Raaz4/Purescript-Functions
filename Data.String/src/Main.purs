module Main where

import Prelude
import Effect (Effect)
import Effect.Class.Console (logShow)
import Data.String
import Data.Maybe

-- array :: Array CodePoint
-- array = [CodePoint 0x62, CodePoint 0x20, CodePoint 0x1D400, CodePoint 0x1D400]

uncons1 :: String -> Maybe { head :: CodePoint, tail :: String }
uncons1 s = uncons s

toCodePointArray1 :: String -> Array CodePoint
toCodePointArray1 s = toCodePointArray s

takeWhile1 :: (CodePoint -> Boolean) -> String -> String
takeWhile1 c s = takeWhile c s

take1 :: Int -> String -> String
take1 i s = take i s

stripSuffix1 :: Pattern -> String -> Maybe String
stripSuffix1 p s = stripSuffix p s

stripPrefix1 :: Pattern -> String -> Maybe String
stripPrefix1 p s = stripPrefix p s

splitAt1 :: Int -> String -> { before :: String, after :: String }
splitAt1 i s = splitAt i s

singleton1 :: CodePoint -> String
singleton1 c = singleton c

length1 :: String -> Int
length1 s = length s

lastIndexOf'1 :: Pattern -> Int -> String -> Maybe Int
lastIndexOf'1 p i s = lastIndexOf' p i s

lastIndexOf1 :: Pattern -> String -> Maybe Int
lastIndexOf1 p s = lastIndexOf p s

indexOf'1 :: Pattern -> Int -> String -> Maybe Int
indexOf'1 p i s = indexOf' p i s

indexOf1 :: Pattern -> String -> Maybe Int
indexOf1 p s = indexOf p s

fromCodePointArray1 :: Array CodePoint -> String
fromCodePointArray1 array = fromCodePointArray array

dropWhile1 :: (CodePoint -> Boolean) -> String -> String
dropWhile1 c s = dropWhile c s

drop1 :: Int -> String -> String
drop1 i s = drop i s

countPrefix1 :: (CodePoint -> Boolean) -> String -> Int
countPrefix1 c s = countPrefix c s

contains1 :: Pattern -> String -> Boolean
contains1 p s = contains p s

codePointFromChar1 :: Char -> CodePoint
codePointFromChar1 c = codePointFromChar c

codePointAt1 :: Int -> String -> Maybe CodePoint
codePointAt1 i s = codePointAt i s

trim1 :: String -> String
trim1 s = trim s

toUpper1 :: String -> String
toUpper1 s = toUpper s

toLower1 :: String -> String
toLower1 s = toLower s

split1 :: Pattern -> String -> Array String
split1 p s = split p s

replaceAll1 :: Pattern -> Replacement -> String -> String
replaceAll1 p r s = replaceAll p r s

replace1 :: Pattern -> Replacement -> String -> String
replace1 p r s = replace p r s

null1 :: String -> Boolean
null1 s = null s

localeCompare1 :: String -> String -> Ordering
localeCompare1 s st = localeCompare s st

joinWith1 :: String -> Array String -> String
joinWith1 s a = joinWith s a

main :: Effect Unit
main = do
  logShow $ uncons1 "uncons"
  logShow $ toCodePointArray1 "toCodePointArray"
  -- logShow $ takeWhile1 (\c -> codePointToInt c == 0x1D400) "takeWhile"
  logShow $ take1 3 "take"
  logShow $ stripSuffix1 (Pattern "ix") "stripSuffix"
  logShow $ stripPrefix1 (Pattern "s") "stripPrefix"
  logShow $ splitAt1 4 "splitAt"
  -- logShow $ singleton1 0x1D400
  logShow $ length1 "length"
  logShow $ lastIndexOf'1 (Pattern "t") 3 "lastIndexOf'"
  logShow $ lastIndexOf1 (Pattern "@") "l@astIndexOf"
  logShow $ indexOf'1 (Pattern "e") 3 "indexOf'"
  logShow $ indexOf1 (Pattern "&") "index&Of"
  -- logShow $ fromCodePointArray1 array
  -- logShow $ dropWhile1 0x1D400 "dropWhile"
  logShow $ drop 2 "drop"
  -- logShow $ countPrefix1 0x1D400 "countPrefix1"
  logShow $ contains1 (Pattern "$") "contain$"
  logShow $ codePointFromChar1 's'
  logShow $ codePointAt1 3 "codePointAt"
  logShow $ trim1 "trim"
  logShow $ toUpper1 "toupper"
  logShow $ toLower1 "TOLOWER"
  logShow $ split1 (Pattern "l") "split"
  logShow $ replaceAll1 (Pattern "a") (Replacement "@") "replaceall"
  logShow $ replace1 (Pattern "a") (Replacement "@") "replace"
  logShow $ null "null"
  logShow $ localeCompare1 "localeCompare1" "localeCompare"
  logShow $ joinWith1 "&" ["join","With"]
  
