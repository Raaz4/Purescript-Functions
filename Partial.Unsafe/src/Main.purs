module Main where

import Prelude
import Effect (Effect)
import Partial.Unsafe
import Effect.Console (logShow)

unsafePartial1 :: forall a. (Partial => a) -> a
unsafePartial1 p = unsafePartial p

unsafePartialBecause1 :: forall a. String -> (Partial => a) -> a
unsafePartialBecause1 s p = unsafePartialBecause s p

unsafeCrashWith1 :: forall a. String -> a
unsafeCrashWith1 s = unsafeCrashWith s

main :: Effect Unit
main = do
  logShow $ unsafePartial1 5
  logShow $ unsafePartialBecause1 "string" 7
  logShow $ unsafeCrashWith1 "error" :: String
