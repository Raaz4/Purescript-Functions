module Main where

import Prelude
import Effect (Effect)
import Effect.Class.Console (logShow)
import Control.Applicative

quot :: Int -> Int -> Int
quot a b = a / b

array = [1,2,5,4,6,7,4,9,8,5,3,8]

-- apply1 :: forall a b. f (a -> b) -> f a -> f b
-- apply1 a b = apply a b

main :: Effect Unit
main = do
  logShow (quot <$> array <*> array)
  -- logShow (apply1 (quot <$> array) array)
