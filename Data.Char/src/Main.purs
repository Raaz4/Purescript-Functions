module Main where

import Data.Char (toCharCode, fromCharCode)
import Effect (Effect)
import Effect.Class.Console (logShow, log)
import Prelude

main :: Effect Unit
main = do
  log $ "fromCharCode : Int -> Maybe Char"
  logShow $ fromCharCode 97
  log $ "toCharCode : Char -> Int"
  logShow $ toCharCode '1'
  
