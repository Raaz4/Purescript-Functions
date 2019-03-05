module Main where

import Prelude (class Ord, class Ring, Ordering, Unit, between, clamp, comparing, discard, max, min, ($), (<), (<=), (>), (>=))
import Effect (Effect)
import Effect.Class.Console (logShow, log)
import Data.Ord (abs, greaterThan, greaterThanOrEq, lessThan, lessThanOrEq, signum)

-- Instances
-- Ord Boolean
-- Ord Int
-- Ord Number
-- Ord String
-- Ord Char
-- Ord Unit
-- Ord Void
-- (Ord a) => Ord (Array a)
-- Ord Ordering
-- (RowToList row list, OrdRecord list row) => Ord {  | row }

main :: Effect Unit
main = do
  log $ "lessThan :: forall a. Ord a => a -> a -> Boolean"
  logShow $ lessThan 3 4
  log $ "lessThanOrEq :: forall a. Ord a => a -> a -> Boolean"
  logShow $ lessThanOrEq 4 4
  log $ "greaterThan :: forall a. Ord a => a -> a -> Boolean"
  logShow $ greaterThan 3 4
  log $ "greaterThanOrEq :: forall a. Ord a => a -> a -> Boolean"
  logShow $ greaterThanOrEq 3 4
  log $ "comparing :: forall a b. Ord b => (a -> b) -> (a -> a -> Ordering)"
  -- logShow $ comparing (\a -> a > 4)
  log $ "min :: forall a. Ord a => a -> a -> a"
  logShow $ min 3 4
  log $ "max :: forall a. Ord a => a -> a -> a"
  logShow $ max 4 3
  log $ "clamp :: forall a. Ord a => a -> a -> a -> a"
  logShow $ clamp 1 2 3
  log $ "between :: forall a. Ord a => a -> a -> a -> Boolean"
  logShow $ between 1 4 3
  log $ "abs :: forall a. Ord a => Ring a => a -> a"
  logShow $ abs 4
  log $ "signum :: forall a. Ord a => Ring a => a -> a"
  logShow $ signum 4
  logShow $ "------------ Operators -------------"
  logShow $ 4 < 8 -- Operator alias for Data.Ord.lessThan (left-associative / precedence 4)
  logShow $ 5 <= 5 -- Operator alias for Data.Ord.lessThanOrEq (left-associative / precedence 4)
  logShow $ 7 > 0 -- Operator alias for Data.Ord.greaterThan (left-associative / precedence 4)
  logShow $ 6 >= 6 -- Operator alias for Data.Ord.greaterThanOrEq (left-associative / precedence 4)




