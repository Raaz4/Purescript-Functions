module Main where

import Prelude (Unit, discard, ($))
import Effect (Effect)
import Effect.Class.Console (logShow, log)
import Data.String (Pattern(..), Replacement(..), codePointAt, codePointFromChar, contains, drop, indexOf, indexOf', joinWith, lastIndexOf, lastIndexOf', length, localeCompare, null, replace, replaceAll, split, splitAt, stripPrefix, stripSuffix, take, toCodePointArray, toLower, toUpper, trim, uncons)

-- array :: Array CodePoint
-- array = [CodePoint 0x62, CodePoint 0x20, CodePoint 0x1D400, CodePoint 0x1D400]

main :: Effect Unit
main = do
    log $ "uncons :: String -> Maybe { head :: CodePoint, tail :: String }"
    logShow $ uncons "uncons"
    log $ "toCodePointArray :: String -> Array CodePoint"
    logShow $ toCodePointArray "toCodePointArray"
    log $ "takeWhile :: (CodePoint -> Boolean) -> String -> String"
    -- logShow $ takeWhile (\c -> codePointToInt c == 0x1D400) "takeWhile"
    log $ "take :: Int -> String -> String"
    logShow $ take 3 "take"
    log $ "stripSuffix :: Pattern -> String -> Maybe String"
    logShow $ stripSuffix (Pattern "ix") "stripSuffix"
    log $ "stripPrefix :: Pattern -> String -> Maybe String"
    logShow $ stripPrefix (Pattern "s") "stripPrefix"
    log $ "splitAt :: Int -> String -> { before :: String, after :: String }"
    logShow $ splitAt 4 "splitAt"
    log $ "singleton :: CodePoint -> String"
    -- logShow $ singleton 0x1D400
    log $ "length :: String -> Int"
    logShow $ length "length"
    log $ "lastIndexOf' :: Pattern -> Int -> String -> Maybe Int"
    logShow $ lastIndexOf' (Pattern "t") 3 "lastIndexOf'"
    log $ "lastIndexOf :: Pattern -> String -> Maybe Int"
    logShow $ lastIndexOf (Pattern "@") "l@astIndexOf"
    log $ "indexOf' :: Pattern -> Int -> String -> Maybe Int"
    logShow $ indexOf' (Pattern "e") 3 "indexOf'"
    log $ "indexOf :: Pattern -> String -> Maybe Int"
    logShow $ indexOf (Pattern "&") "index&Of"
    log $ "fromCodePointArray :: Array CodePoint -> String"
    -- logShow $ fromCodePointArray array
    log $ "dropWhile :: (CodePoint -> Boolean) -> String -> String"
    -- logShow $ dropWhile 0x1D400 "dropWhile"
    log $ "drop :: Int -> String -> String"
    logShow $ drop 2 "drop"
    log $ "countPrefix :: (CodePoint -> Boolean) -> String -> Int"
    -- logShow $ countPrefix 0x1D400 "countPrefix1"
    log $ "contains :: Pattern -> String -> Boolean"
    logShow $ contains (Pattern "$") "contain$"
    log $ "codePointFromChar :: Char -> CodePoint"
    logShow $ codePointFromChar 's'
    log $ "codePointAt :: Int -> String -> Maybe CodePoint"
    logShow $ codePointAt 3 "codePointAt"
    log $ "trim :: String -> String"
    logShow $ trim "trim"
    log $ "toUpper :: String -> String"
    logShow $ toUpper "toupper"
    log $ "toLower :: String -> String"
    logShow $ toLower "TOLOWER"
    log $ "split :: Pattern -> String -> Array String"
    logShow $ split (Pattern "l") "split"
    log $ "replaceAll :: Pattern -> Replacement -> String -> String"
    logShow $ replaceAll (Pattern "a") (Replacement "@") "replaceall"
    log $ "replace :: Pattern -> Replacement -> String -> String"
    logShow $ replace (Pattern "a") (Replacement "@") "replace"
    log $ "null :: String -> Boolean"
    logShow $ null "null"
    log $ "localeCompare :: String -> String -> Ordering"
    logShow $ localeCompare "localeCompare1" "localeCompare"
    log $ "joinWith :: String -> Array String -> String"
    logShow $ joinWith "&" ["join","With"]
    
