module Main where

import Prelude (Unit, discard, ($))
import Effect (Effect)
import Effect.Class.Console (logShow, log)
import Math (abs, acos, asin, atan, atan2, ceil, cos, e, exp, floor, ln10, ln2, log, log10e, log2e, max, min, pi, pow, remainder, round, sin, sqrt, sqrt1_2, sqrt2, tan, tau, trunc, (%)) as M

main :: Effect Unit
main = do
  log $ "abs :: Number -> Number"
  logShow $ M.abs 80.90
  log $ "acos :: Number -> Radians"
  logShow $ M.acos 1.00
  log $ "asin :: Number -> Radians"
  logShow $ M.asin 0.50
  log $ "atan :: Number -> Radians"
  logShow $ M.atan 0.60
  log $ "atan2 :: Number -> Number -> Radians"
  logShow $ M.atan2 45.00 45.00
  log $ "ceil :: Number -> Number"
  logShow $ M.ceil 80.90
  log $ "cos :: Radians -> Number"
  logShow $ M.cos 5.77
  log $ "exp :: Number -> Number"
  logShow $ M.exp 0.0
  log $ "floor :: Number -> Number"
  logShow $ M.floor 80.90
  log $ "log :: Number -> Number"
  logShow $ M.log 10.0
  log $ "max :: Number -> Number -> Number"
  logShow $ M.max 89.03 87.44
  log $ "min :: Number -> Number -> Number"
  logShow $ M.min 454.32 374.303
  log $ "pow :: Number -> Number -> Number"
  logShow $ M.pow 4.0 3.0
  log $ "round :: Number -> Number"
  logShow $ M.round 45.66
  log $ "sin :: Radians -> Number"
  logShow $ M.sin 6.77
  log $ "sqrt :: Number -> Number"
  logShow $ M.sqrt 45.33
  log $ "tan :: Radians -> Number"
  logShow $ M.tan 7.99
  log $ "trunc :: Number -> Number"
  logShow $ M.trunc 80.90
  log $ "remainder :: Number -> Number -> Number"
  logShow $ M.remainder 67.55 54.3
  logShow $ "------------ Constant Values -------------"
  logShow $ M.e -- The base of natural logarithms, e, around 2.71828.
  logShow $ M.ln2 -- The natural logarithm of 2, around 0.6931.
  logShow $ M.ln10 -- The natural logarithm of 10, around 2.3025.
  logShow $ M.log2e -- The base 2 logarithm of e, around 1.4426.
  logShow $ M.log10e -- Base 10 logarithm of e, around 0.43429.
  logShow $ M.pi -- The ratio of the circumference of a circle to its diameter, around 3.14159.
  logShow $ M.tau -- The ratio of the circumference of a circle to its radius, around 6.283185.
  logShow $ M.sqrt1_2 -- The Square root of one half, around 0.707107.
  logShow $ M.sqrt2 -- The square root of two, around 1.41421.
  logShow $ "------------ Operators -------------"
  logShow $ 453.32 M.% 54.44 -- Operator alias for Math.remainder (left-associative / precedence 7)


