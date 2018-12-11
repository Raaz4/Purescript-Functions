module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Class.Console (logShow)
import Math (abs, ceil, exp, floor, log, max, pow, trunc)

main :: Effect Unit
main = do
  logShow (abs (80.90))
  logShow (ceil (80.90))
  logShow (exp 0.0)
  logShow (floor (80.90))
  logShow (log 10.0)
  logShow (max 89.03 87.44)
  logShow (min 454.32 374.303)
  logShow (pow 4.0 3.0)
  logShow (trunc 80.90)
