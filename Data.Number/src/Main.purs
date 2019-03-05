module Main where

import Prelude (Unit, discard, unit, ($))
import Effect (Effect)
import Data.Number (fromString, infinity, isFinite, isNaN, nan)
import Effect.Class.Console (logShow)
import Data.Maybe (Maybe)

main :: Effect Unit
main = do
  log $ "fromString :: String -> Maybe Number"
  logShow $ fromString "100.00"
  log $ "nan1 :: Number"
  logShow $ nan
  log $ "isNaN :: Number -> Boolean"
  logShow $ isNaN nan
  log $ "infinity :: Number"
  logShow $ infinity
  log $ "isFinite :: Number -> Boolean"
  logShow $ isFinite 30.39

