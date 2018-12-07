module Main where

import Prelude
import Effect (Effect)
import Effect.Class.Console (logShow)
import Data.Unfoldable
import Data.Maybe
import Data.Tuple

-- replicate1 :: forall f a. Unfoldable f => Int -> a -> f a
-- replicate1 a b = replicate a b

replicate :: forall f a. Unfoldable f => Int -> a -> f a
replicate n v = unfoldr step n
  where
    step :: Int -> Maybe (Tuple a Int)
    step i =
      if i <= 0 then Nothing
      else Just (Tuple v (i - 1))

main :: Effect _
main = do
  logShow (replicate 4 "in")
