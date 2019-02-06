module Main where

import Prelude
import Effect (Effect)
import Effect.Class.Console (logShow)
import Control.Bind
import Data.Int
import Data.Maybe
import Data.Array

-- Instances
-- Bind (Function r)
-- Bind Array

arraymNum :: Array Number
arraymNum = [2.44, 6.55]

arrayint :: Array Int
arrayint = [2,1,5,4,3,6,4,2]

bind1 :: forall m a b. Bind m => m a -> (a -> m b) -> m b
bind1 m f = bind m f

bindFlipped1 :: forall m a b. Bind m => (a -> m b) -> m a -> m b
bindFlipped1 f m = bindFlipped f m

join1 :: forall a m. Bind m => m (m a) -> m a
join1 m = join m

composeKleisli1 :: forall a b c m. Bind m => (a -> m b) -> (b -> m c) -> a -> m c
composeKleisli1 f1 f2 a = composeKleisli f1 f2 a

composeKleisliFlipped1 :: forall a b c m. Bind m => (b -> m c) -> (a -> m b) -> a -> m c
composeKleisliFlipped1 f1 f2 a = composeKleisliFlipped f1 f2 a

ifM1 :: forall a m. Bind m => m Boolean -> m a -> m a -> m a
ifM1 mb ma m = ifM mb ma m

main :: Effect Unit
main = do
  logShow $ bind1 (Just 90.00) fromNumber
  logShow $ bindFlipped1 fromNumber (Just 89.00)
  logShow $ join1 $ Just (Just 8)
  logShow $ composeKleisli1 tail tail arrayint
  logShow $ composeKleisliFlipped1 tail tail arrayint
  logShow $ ifM1 (Just true) (Just 9) (Just 100)
  logShow $ ifM1 (Just false) (Just 9) (Just 100)
  logShow $ "------------ Operators -------------"
  logShow $ (Just 9.0) >>= fromNumber -- Operator alias for Control.Bind.bind (left-associative / precedence 1)
  logShow $ fromNumber =<< (Just 45.00) -- Operator alias for Control.Bind.bindFlipped (right-associative / precedence 1)
  logShow $ (tail >=> tail >=> head) arrayint -- Operator alias for Control.Bind.composeKleisli (right-associative / precedence 1)
  logShow $ (>=>) (\x -> [x+1,x+2,x+3]) (\y -> [y-1,y-2,y-3]) 5
  logShow $ (head <=< tail <=< tail) arrayint -- Operator alias for Control.Bind.composeKleisliFlipped (right-associative / precedence 1)
  logShow $ (<=<) (\x -> [x+1,x+2,x+3]) (\y -> [y-1,y-2,y-3]) 5

