module Main where

import Prelude
import Effect (Effect)
import Data.Number
import Effect.Class.Console (logShow)
import Data.Maybe

fromString1 :: String -> Maybe Number
fromString1 s = fromString s

nan1 :: Number
nan1 = nan

isNaN1 :: Number -> Boolean
isNaN1 n = isNaN n

infinity1 :: Number
infinity1 = infinity

isFinite1 :: Number -> Boolean
isFinite1 f = isFinite f

main :: Effect Unit
main = do
  logShow $ fromString1 "100.00"
  logShow $ nan1
  logShow $ isNaN1 nan
  logShow $ infinity1
  logShow $ isFinite1 30.39

