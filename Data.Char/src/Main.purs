module Main where

import Prelude
import Effect (Effect)
import Effect.Class.Console (logShow, log)
import Data.Char (toCharCode, fromCharCode)

main :: Effect Unit
main = do
  log $ "toCharCode : Char -> Int"
  logShow $ toCharCode '1'
  log $ "fromCharCode : Int -> Maybe Char"
  logShow $ fromCharCode 97
