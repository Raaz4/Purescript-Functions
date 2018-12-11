module Main where

import Prelude
import Effect (Effect)
import Effect.Class.Console (logShow)
import Control.Monad
import Data.Maybe
import Data.Int

array :: Array Int
array = [1,2,34,5,6,7,8,95,43,32,22,12]
maybe = Just(Just 3)

join1 :: forall a m. Bind m => m (m a) -> m a
join1 a = join a

void1 :: forall f a. Functor f => f a -> f Unit
void1 array = void array

liftM11 :: forall m a b. Monad m => (a -> b) -> m a -> m b
liftM11 f m = liftM1 f m

ap1 :: forall m a b. Monad m => m (a -> b) -> m a -> m b
ap1 mf m = ap mf m

whenM1 :: forall m. Monad m => m Boolean -> m Unit -> m Unit
whenM1 mB mU = whenM mB mU

unlessM1 :: forall m. Monad m => m Boolean -> m Unit -> m Unit
unlessM1 mB mU = unlessM mB mU

main :: Effect Unit
main = do
  logShow $ join1 maybe
  logShow $ Just 2.0 >>= fromNumber
  logShow $ void1 array
  logShow $ liftM11 ceil $ Just 4.9
  logShow $ ap1 $ Just ceil $ Just 5.6
  logShow $ whenM1 $ Just true $ Just unit
  logShow $ unlessM1 $ Just true $ Just unit
