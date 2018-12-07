module Main where

import Prelude
import Effect (Effect)
import Data.Either
import Control.Alt
import Effect.Class.Console (logShow)

either1 :: forall a b c. (a -> c) -> (b -> c) -> Either a b -> c
either1 a b c = either a b c

choose1 :: forall m a b. Alt m => m a -> m b -> m (Either a b)
choose1 a b = choose a b

isLeft1 :: forall a b. Either a b -> Boolean
isLeft1 a = isLeft a

isRight1 :: forall a b. Either a b -> Boolean
isRight1 a = isRight a

fromLeft1 :: forall a b. Partial => Either a b -> a
fromLeft1 a = fromLeft a

note1 :: forall a b. a -> Maybe b -> Either a b
note1 a b = note a b

hush1 :: forall a b. Either a b -> Maybe b
hush1 a = hush a

main :: Effect Unit
main = do
  -- logShow (isLeft1 (Either 1 4))
  -- logShow (fromLeft1 (Either 9 4))
