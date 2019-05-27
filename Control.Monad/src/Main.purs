module Main where

import Data.Int (ceil)
import Data.Maybe (Maybe (..))
import Effect (Effect)
import Effect.Class.Console (logShow, log)
import Prelude (class Bind, class Functor, class Monad, Unit, ap, discard, join, liftM1, unit, unlessM, void, whenM, ($))

-- Instances
-- Monad (Function r)
-- Monad Array

array :: Array Int
array = [1,2,34,5,6,7,8,95,43,32,22,12]

maybe :: Maybe (Maybe Int)
maybe = Just(Just 3)

main :: Effect Unit
main = do
  log $ "ap : forall m a b. Monad m => m (a -> b) -> m a -> m b"
  logShow $ ap (Just ceil) (Just 5.6)
  log $ "join : forall a m. Bind m => m (m a) -> m a"
  logShow $ join (Just (Just 5))
  log $ "liftM1 : forall m a b. Monad m => (a -> b) -> m a -> m b"
  logShow $ liftM1 ceil $ Just 4.9
  log $ "unlessM : forall m. Monad m => m Boolean -> m Unit -> m Unit"
  logShow $ unlessM (Just true) (Just unit)
  logShow $ unlessM (Just false) (Just unit)
  log $ "void : forall f a. Functor f => f a -> f Unit"
  logShow $ void array
  log $ "when : forall m. Monad m => m Boolean -> m Unit -> m Unit"
  logShow $ whenM (Just false) (Just unit)
  logShow $ whenM (Just true) (Just unit)
  
