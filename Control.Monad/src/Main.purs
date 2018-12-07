module Main where

import Prelude
import Effect (Effect)
import Effect.Class.Console (logShow)
import Control.Monad
import Data.Maybe

array :: Array Int
array = [1,2,34,5,6,7,8,95,43,32,22,12]
arraymay = [(Just 1),(Just 3),(Just 5),(Just 55),Nothing]

join1 :: forall a m. Bind m => m (m a) -> m a
join1 a = join a

main :: Effect Unit
main = do
  -- logShow (join1 array)
