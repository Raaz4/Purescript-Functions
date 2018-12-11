module Main where

import Prelude
import Effect (Effect)
import Effect.Class.Console (logShow)
import Data.String
import Data.Maybe

take1 :: Int -> String -> String
take1 i s = take i s

drop1 :: Int -> String -> String
drop1 i s = drop i s

stripPrefix1 :: Pattern -> String -> Maybe String
stripPrefix1 p s = stripPrefix p s

length1 :: String -> Int
length1 s = length s

splitAt1 :: Int -> String -> { before :: String, after :: String }
splitAt1 i s = splitAt i s

joinWith1 :: String -> Array String -> String
joinWith1 s a = joinWith s a

indexOf1 :: Pattern -> String -> Maybe Int
indexOf1 p s = indexOf p s

contains1 :: Pattern -> String -> Boolean
contains1 p s = contains p s

toUpper1 :: String -> String
toUpper1 s = toUpper s

replace1 :: Pattern -> Replacement -> String -> String
replace1 p r s = replace p r s

null1 :: String -> Boolean
null1 s = null s

lastIndexOf1 :: Pattern -> String -> Maybe Int
lastIndexOf1 p s = lastIndexOf p s

main :: Effect Unit
main = do
  logShow $ take1 3 "take"
  logShow $ drop 2 "drop"
  logShow $ stripPrefix1 (Pattern "s") "stripPrefix"
  logShow $ length1 "length"
  logShow $ splitAt1 4 "splitAt"
  logShow $ joinWith1 "&" ["join","With"]
  logShow $ indexOf1 (Pattern "&") "index&Of"
  logShow $ contains1 (Pattern "$") "contain$"
  logShow $ toUpper1 "toUpper"
  logShow $ replace1 (Pattern "a") (Replacement "@") "replace"
  logShow $ null "null"
  logShow $ lastIndexOf1 (Pattern "@") "l@astIndexOf"
