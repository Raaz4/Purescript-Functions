module Main where

import Prelude (class Bind, Unit, bind, discard, ifM, join, ($), (+), (-), (<=<), (=<<), (>=>), (>>=))
import Effect (Effect)
import Effect.Class.Console (logShow, log)
import Control.Bind (bindFlipped, composeKleisli, composeKleisliFlipped)
import Data.Int (fromNumber)
import Data.Maybe (Maybe (..))
import Data.Array (head, tail)

-- Instances
-- Bind (Function r)
-- Bind Array

arraymNum :: Array Number
arraymNum = [2.44, 6.55]

arrayint :: Array Int
arrayint = [2,1,5,4,3,6,4,2]

main :: Effect Unit
main = do
  log $ "bind : forall m a b. Bind m => m a -> (a -> m b) -> m b"
  logShow $ bind (Just 90.00) fromNumber
  log $ "\tbindflipped : forall m a b. Bind m => (a -> m b) -> m a -> m b"
  logShow $ bindFlipped fromNumber (Just 89.00)
  log $ "join : forall a m. Bind m => m (m a) -> m a"
  logShow $ join $ Just (Just 8)
  log $ "composeKleisli : forall a b c m. Bind m => (a -> m b) -> (b -> m c) -> a -> m c"
  logShow $ composeKleisli tail tail arrayint
  log $ "composeKleisliFlipped : forall a b c m. Bind m => (b -> m c) -> (a -> m b) -> a -> m c"
  logShow $ composeKleisliFlipped tail tail arrayint
  log $ "ifM : forall a m. Bind m => m Boolean -> m a -> m a -> m a"
  logShow $ ifM (Just true) (Just 9) (Just 100)
  logShow $ ifM (Just false) (Just 9) (Just 100)
  log $ "\n------------ Operators -------------\n"
  logShow $ (Just 9.0) >>= fromNumber -- Operator alias for Control.Bind.bind (left-associative / precedence 1)
  logShow $ fromNumber =<< (Just 45.00) -- Operator alias for Control.Bind.bindFlipped (right-associative / precedence 1)
  logShow $ (tail >=> tail >=> head) arrayint -- Operator alias for Control.Bind.composeKleisli (right-associative / precedence 1)
  logShow $ (>=>) (\x -> [x+1,x+2,x+3]) (\y -> [y-1,y-2,y-3]) 5
  logShow $ (head <=< tail <=< tail) arrayint -- Operator alias for Control.Bind.composeKleisliFlipped (right-associative / precedence 1)
  logShow $ (<=<) (\x -> [x+1,x+2,x+3]) (\y -> [y-1,y-2,y-3]) 5

