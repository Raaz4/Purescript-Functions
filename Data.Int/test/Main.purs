module Test.Main where

import Prelude
import Main
import Effect (Effect)
import Effect.Class.Console (logShow)
import Data.Int

main :: Effect _
main = do
       logShow (round1 6.0)
       logShow (even1 5)
       logShow (odd1 4)
       logShow (fromNumber1 11.0)
       logShow (fromString1 "11")
       logShow (toNumber1 9)
       logShow (quot1 2 3)
       logShow (rem1 1 3)
       logShow (pow1 2 2)