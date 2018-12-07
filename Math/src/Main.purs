module Main where

import Prelude
import Effect (Effect)
import Effect.Class.Console (logShow)
import Math (Radians, abs, ceil, exp, floor, log, max, min, pow, trunc) as M

type Radians = Number

abs :: Number -> Number
abs a = M.abs a

ceil :: Number -> Number
ceil c = M.ceil c

exp :: Number -> Number
exp e = M.exp e

floor :: Number -> Number
floor f = M.floor f

log :: Number -> Number
log l = M.log l

max :: Number -> Number -> Number
max a b = M.max a b

min :: Number -> Number -> Number
min a b = M.min a b

pow :: Number -> Number -> Number
pow x n = M.pow x n

trunc :: Number -> Number
trunc t = M.trunc t

