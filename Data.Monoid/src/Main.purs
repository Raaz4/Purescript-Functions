module Main where

import Prelude
import Effect (Effect)
import Effect.Class.Console (logShow)
import Data.Monoid

-- Instances
-- Monoid Unit
-- Monoid Ordering
-- (Monoid b) => Monoid (a -> b)
-- Monoid String
-- Monoid (Array a)
-- (RowToList row list, MonoidRecord list row row) => Monoid {  | row }

arrayint :: Array Int
arrayint = [1,2,3,4,5,6,7,8,9]

power1 :: forall m. Monoid m => m -> Int -> m
power1 m i = power m i

guard1 :: forall m. Monoid m => Boolean -> m -> m
guard1 b m = guard b m

main :: Effect Unit
main = do
  logShow $ power1 "@" 9
  logShow $ power1 arrayint 3
  logShow $ power1 unit 7
  logShow $ guard1 true "#"
  logShow $ guard1 false "#"
  logShow $ guard1 true arrayint
  logShow $ guard1 false arrayint
