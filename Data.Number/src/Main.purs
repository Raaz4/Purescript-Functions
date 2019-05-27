module Main where

import Data.Maybe (Maybe)
import Data.Number (fromString, infinity, isFinite, isNaN, nan)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Prelude (Unit, discard, unit, ($))

main :: Effect Unit
main = do
  log $ "fromString :: String -> Maybe Number"
  logShow $ fromString "100.00"
  log $ "infinity :: Number"
  logShow $ infinity
  log $ "isFinite :: Number -> Boolean"
  logShow $ isFinite 30.39
  log $ "isNaN :: Number -> Boolean"
  logShow $ isNaN nan
  log $ "nan1 :: Number"
  logShow $ nan
