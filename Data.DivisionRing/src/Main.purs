module Main where

import Data.DivisionRing (leftDiv, rightDiv)
import Effect (Effect)
import Effect.Class.Console (logShow, log)
import Prelude

-- Instances
-- DivisionRing Number

main :: Effect Unit
main = do
  log $ "leftDiv : forall a. DivisionRing a => a -> a -> a"
  logShow $ leftDiv 6.7 3.22
  log $ "rightDiv : forall a. DivisionRing a => a -> a -> a"
  logShow $ rightDiv 6.7 3.22
