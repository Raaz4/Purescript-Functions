module Main where

import Prelude
import Effect (Effect)
import Effect.Class.Console (logShow)
import Math (min, max)
import Math

abs1 :: Number -> Number
abs1 a = abs a

acos1 :: Number -> Radians
acos1 n = acos n

asin1 :: Number -> Radians
asin1 n = asin n

atan1 :: Number -> Radians
atan1 n = atan n

atan21 :: Number -> Number -> Radians
atan21 n n1 = atan2 n n1

ceil1 :: Number -> Number
ceil1 c = ceil c

cos1 :: Radians -> Number
cos1 r = cos r

exp1 :: Number -> Number
exp1 e = exp e

floor1 :: Number -> Number
floor1 f = floor f

log1 :: Number -> Number
log1 l = log l

max1 :: Number -> Number -> Number
max1 a b = max a b

min1 :: Number -> Number -> Number
min1 a b = min a b

pow1 :: Number -> Number -> Number
pow1 x n = pow x n

round1 :: Number -> Number
round1 n = round n

sin1 :: Radians -> Number
sin1 r = sin r

sqrt1 :: Number -> Number
sqrt1 n = sqrt n

tan1 :: Radians -> Number
tan1 r = tan r

trunc1 :: Number -> Number
trunc1 t = trunc t

remainder1 :: Number -> Number -> Number
remainder1 n n1 = remainder n n1

main :: Effect Unit
main = do
  logShow $ abs1 80.90
  logShow $ acos1 1.00
  logShow $ asin1 0.50
  logShow $ atan1 0.60
  logShow $ atan21 45.00 45.00
  logShow $ ceil1 80.90
  logShow $ cos1 5.77
  logShow $ exp1 0.0
  logShow $ floor1 80.90
  logShow $ log1 10.0
  logShow $ max1 89.03 87.44
  logShow $ min1 454.32 374.303
  logShow $ pow1 4.0 3.0
  logShow $ round1 45.66
  logShow $ sin1 6.77
  logShow $ sqrt1 45.33
  logShow $ tan1 7.99
  logShow $ trunc1 80.90
  logShow $ remainder1 67.55 54.3
  logShow $ "------------ Constant Values -------------"
  logShow $ e -- The base of natural logarithms, e, around 2.71828.
  logShow $ ln2 -- The natural logarithm of 2, around 0.6931.
  logShow $ ln10 -- The natural logarithm of 10, around 2.3025.
  logShow $ log2e -- The base 2 logarithm of e, around 1.4426.
  logShow $ log10e -- Base 10 logarithm of e, around 0.43429.
  logShow $ pi -- The ratio of the circumference of a circle to its diameter, around 3.14159.
  logShow $ tau -- The ratio of the circumference of a circle to its radius, around 6.283185.
  logShow $ sqrt1_2 -- The Square root of one half, around 0.707107.
  logShow $ sqrt2 -- The square root of two, around 1.41421.
  logShow $ "------------ Operators -------------"
  logShow $ 453.32 % 54.44 -- Operator alias for Math.remainder (left-associative / precedence 7)


