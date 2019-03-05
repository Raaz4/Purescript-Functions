module Main where

import Prelude
import Effect (Effect)
import Partial.Unsafe (unsafePartial, unsafePartialBecause, unsafeCrashWith)
import Effect.Class.Console (logShow, log)

main :: Effect Unit
main = do
  log $ "unsafePartial :: forall a. (Partial => a) -> a"
  logShow $ unsafePartial 5
  log $ "unsafePartialBecause :: forall a. String -> (Partial => a) -> a"
  logShow $ unsafePartialBecause "string" 7
  log $ "unsafeCrashWith :: forall a. String -> a"
  logShow $ unsafeCrashWith "error" :: String
