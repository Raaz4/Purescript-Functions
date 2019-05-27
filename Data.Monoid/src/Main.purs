module Main where

import Data.Monoid (guard, power)
import Effect (Effect)
import Effect.Class.Console (logShow, log)
import Prelude (class Monoid, Unit, discard, unit, ($))

-- Instances
-- Monoid Unit
-- Monoid Ordering
-- (Monoid b) => Monoid (a -> b)
-- Monoid String
-- Monoid (Array a)
-- (RowToList row list, MonoidRecord list row row) => Monoid {  | row }

arrayint :: Array Int
arrayint = [1,2,3,4,5,6,7,8,9]

main :: Effect Unit
main = do
  log $ "guard :: forall m. Monoid m => Boolean -> m -> m"
  logShow $ guard true "#"
  logShow $ guard false "#"
  logShow $ guard true arrayint
  logShow $ guard false arrayint
  log $ "power :: forall m. Monoid m => m -> Int -> m"
  logShow $ power "@" 9
  logShow $ power arrayint 3
  logShow $ power unit 7
