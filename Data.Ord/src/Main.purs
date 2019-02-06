module Main where

import Prelude
import Effect (Effect)
import Effect.Class.Console (logShow)
import Data.Ord

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

lessThan1 :: forall a. Ord a => a -> a -> Boolean
lessThan1 a b = lessThan a b

lessThanOrEq1 :: forall a. Ord a => a -> a -> Boolean
lessThanOrEq1 a b = lessThanOrEq a b

greaterThan1 :: forall a. Ord a => a -> a -> Boolean
greaterThan1 a b = greaterThan a b

greaterThanOrEq1 :: forall a. Ord a => a -> a -> Boolean
greaterThanOrEq1 a b = greaterThanOrEq a b

comparing1 :: forall a b. Ord b => (a -> b) -> (a -> a -> Ordering)
comparing1 f = comparing f

min1 :: forall a. Ord a => a -> a -> a
min1 a b = min a b

max1 :: forall a. Ord a => a -> a -> a
max1 a b = max a b

clamp1 :: forall a. Ord a => a -> a -> a -> a
clamp1 a b c = clamp a b c

between1 :: forall a. Ord a => a -> a -> a -> Boolean
between1 a b c = between a b c

abs1 :: forall a. Ord a => Ring a => a -> a
abs1 a = abs a

signum1 :: forall a. Ord a => Ring a => a -> a
signum1 a = signum a

main :: Effect Unit
main = do
  logShow $ lessThan1 3 4
  logShow $ lessThan1 4 3
  logShow $ lessThan1 3 3
  logShow $ lessThanOrEq1 4 4
  logShow $ lessThanOrEq1 4 3
  logShow $ lessThanOrEq1 3 4
  logShow $ greaterThan1 3 4
  logShow $ greaterThan1 4 3
  logShow $ greaterThan1 3 3
  logShow $ greaterThanOrEq1 3 4
  logShow $ greaterThanOrEq1 4 3
  logShow $ greaterThanOrEq1 3 3
  -- logShow $ comparing1 (\a -> a > 4)
  logShow $ min1 3 4
  logShow $ max1 4 3
  logShow $ clamp1 1 2 3
  logShow $ between1 1 4 3
  logShow $ abs 4
  logShow $ signum 4


