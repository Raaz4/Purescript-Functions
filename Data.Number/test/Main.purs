module Test.Main where

import Prelude
import Effect (Effect)
import Main
import Effect.Class.Console (logShow)
import Data.Number

main :: Effect Unit
main = do
  logShow (fromString1 "100.00")
  logShow (nan1)
  logShow (isNaN1 nan)
  logShow (infinity1)
  logShow (isFinite1 30.39)
