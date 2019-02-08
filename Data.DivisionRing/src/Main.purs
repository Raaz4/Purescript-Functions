module Main where

import Prelude
import Effect (Effect)
import Effect.Class.Console (logShow)
import Data.DivisionRing (leftDiv, rightDiv)

-- Instances
-- DivisionRing Number

leftDiv1 :: forall a. DivisionRing a => a -> a -> a
leftDiv1 a b = leftDiv a b

rightDiv1 :: forall a. DivisionRing a => a -> a -> a
rightDiv1 a b = rightDiv a b

main :: Effect Unit
main = do
  logShow $ leftDiv1 6.7 3.22
  logShow $ rightDiv1 6.7 3.22
