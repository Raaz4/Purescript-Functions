module Main where

import Prelude
import Effect (Effect)
import Effect.Class.Console (logShow)
import Data.Char (toCharCode, fromCharCode)
import Data.Maybe (Maybe)

toCharCode1 :: Char -> Int
toCharCode1 c = toCharCode c

fromCharCode1 :: Int -> Maybe Char
fromCharCode1 i = fromCharCode i

main :: Effect Unit
main = do
  logShow $ toCharCode1 '1'
  logShow $ fromCharCode1 97
