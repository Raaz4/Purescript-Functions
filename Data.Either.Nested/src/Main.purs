module Main where

import Prelude (Unit, ($), discard, (*), (-), (+), (/))
import Effect (Effect)
import Effect.Class.Console (logShow, log)
import Data.Either.Nested (at1, at10, at2, at3, at4, at5, at6, at7, at8, at9, either1, either10, either2, either3, either4, either5, either6, either7, either8, either9, in1, in10, in2, in3, in4, in5, in6, in7, in8, in9)
import Data.Either (Either(..))

main :: Effect Unit
main = do
  log $ "in1 : forall a z. a -> Either a z"
  logShow $ in1 5 :: Either _ Int
  log $ "in2 : forall a b z. b -> Either a b z"
  logShow $ in2 4 :: Either Int (Either _ Int)
  log $ "in3 : forall a b c z. c -> Either a b c z"
  logShow $ in3 5 :: Either Int (Either Int (Either _ Int))
  log $ "in4 : forall a b c d z. d -> Either a b c d z"
  logShow $ in4 3 :: Either Int (Either Int (Either Int (Either _ Int)))
  log $ "in5 : forall a b c d e z. e -> Either a b c d e z"
  logShow $ in5 9 :: Either Int (Either Int (Either Int (Either Int (Either _ Int))))
  log $ "in6 : forall a b c d e f z. f -> Either a b c d e f z"
  logShow $ in6 3 :: Either Int (Either Int (Either Int (Either Int (Either Int (Either _ Int)))))
  log $ "in7 : forall a b c d e f g z. g -> Either a b c d e f g z"
  logShow $ in7 3 :: Either Int (Either Int (Either Int (Either Int (Either Int (Either Int (Either _ Int))))))
  log $ "in8 : forall a b c d e f g h z. h -> Either a b c d e f g h z"
  logShow $ in8 3 :: Either Int (Either Int (Either Int (Either Int (Either Int (Either Int (Either Int (Either _ Int)))))))
  log $ "in9 : forall a b c d e f g h i z. i -> Either a b c d e f g h i z"
  logShow $ in9 3 :: Either Int (Either Int (Either Int (Either Int (Either Int (Either Int (Either Int (Either Int (Either _ Int))))))))
  log $ "in10 : forall a b c d e f g h i j z. j -> Either a b c d e f g h i j z"
  logShow $ in10 3 :: Either Int (Either Int (Either Int (Either Int (Either Int (Either Int (Either Int (Either Int (Either Int (Either _ Int)))))))))
  log $ "at1 : forall r a z. r -> (a -> r) -> a  z -> r"
  logShow $ at1 61 (\a -> a*a) (Left 4)
  log $ "at2 : forall r a b z. r -> (b -> r) -> a  b  z -> r"
  logShow $ at2 61 (\b -> b*b) (Right (Left 8))
  log $ "at3 : forall r a b c z. r -> (c -> r) -> a  b  c  z -> r"
  logShow $ at3 41 (\c -> c*c) (Right (Right (Left 9)))
  log $ "at4 : forall r a b c d z. r -> (d -> r) -> a  b  c  d  z -> r"
  logShow $ at4 51 (\d -> d*d) (Right (Right (Right (Left 31))))
  log $ "at5 : forall r a b c d e z. r -> (e -> r) -> a  b  c  d  e  z -> r"
  logShow $ at5 71 (\e -> e*e) (Right (Right (Right (Right (Left 11)))))
  log $ "at6 : forall r a b c d e f z. r -> (f -> r) -> a  b  c  d  e  f  z -> r"
  logShow $ at6 21 (\f -> f*f) (Right (Right (Right (Right (Right (Left 54))))))
  log $ "at7 : forall r a b c d e f g z. r -> (g -> r) -> a  b  c  d  e  f  g  z -> r"
  logShow $ at7 61 (\g -> g*g) (Right (Right (Right (Right (Right (Right (Left 81)))))))
  log $ "at8 : forall r a b c d e f g h z. r -> (h -> r) -> a  b  c  d  e  f  g  h  z -> r"
  logShow $ at8 16 (\h -> h*h) (Right (Right (Right (Right (Right (Right (Right (Left 18))))))))
  log $ "at9 : forall r a b c d e f g h i z. r -> (i -> r) -> a  b  c  d  e  f  g  h  i  z -> r"
  logShow $ at9 60 (\i -> i*i) (Right (Right (Right (Right (Right (Right (Right (Right (Left 80)))))))))
  log $ "at10 : forall r a b c d e f g h i j z. r -> (j -> r) -> a  b  c  d  e  f  g  h  i  j  z -> r"
  logShow $ at10 66 (\j -> j*j) (Right (Right (Right (Right (Right (Right (Right (Right (Right (Left 90))))))))))
  log $ "either1 : forall a. Either1 a -> a"
  logShow $ either1 (Left 400)
  log $ "either2 : forall r a b. (a -> r) -> (b -> r) -> Either2 a b -> r"
  logShow $ either2 (\a -> a-6) (\b -> b+89) (Right (Left 88))
  log $ "either3 : forall r a b c. (a -> r) -> (b -> r) -> (c -> r) -> Either3 a b c -> r"
  logShow $ either3 (\a -> a-6) (\b -> b+89) (\c -> c+55) (Right (Right (Left 88)))
  log $ "either4 : forall r a b c d. (a -> r) -> (b -> r) -> (c -> r) -> (d -> r) -> Either4 a b c d -> r"
  logShow $ either4 (\a -> a-6) (\b -> b+89) (\c -> c+55) (\d -> d/2) (Right (Right (Right (Left 88))))
  log $ "either5 : forall r a b c d e. (a -> r) -> (b -> r) -> (c -> r) -> (d -> r) -> (e -> r) -> Either5 a b c d e -> r"
  logShow $ either5 (\a -> a-6) (\b -> b+89) (\c -> c+55) (\d -> d/2) (\e -> e+23) (Right (Right (Right (Right (Left 88)))))
  log $ "either6 : forall r a b c d e f. (a -> r) -> (b -> r) -> (c -> r) -> (d -> r) -> (e -> r) -> (f -> r) -> Either6 a b c d e f -> r"
  logShow $ either6 (\a -> a-6) (\b -> b+89) (\c -> c+55) (\d -> d/2) (\e -> e+45) (\f -> f-33) (Right (Right (Right (Right (Right (Left 88))))))
  log $ "either7 : forall r a b c d e f g. (a -> r) -> (b -> r) -> (c -> r) -> (d -> r) -> (e -> r) -> (f -> r) -> (g -> r) -> Either7 a b c d e f g -> r"
  logShow $ either7 (\a -> a-6) (\b -> b+89) (\c -> c+55) (\d -> d/2) (\e -> e+45) (\f -> f-33) (\g -> g-6) (Right (Right (Right (Right (Right (Right (Left 88)))))))
  log $ "either8 : forall r a b c d e f g h. (a -> r) -> (b -> r) -> (c -> r) -> (d -> r) -> (e -> r) -> (f -> r) -> (g -> r) -> (h -> r) -> Either8 a b c d e f g h -> r"
  logShow $ either8 (\a -> a-6) (\b -> b+89) (\c -> c+55) (\d -> d/2) (\e -> e+45) (\f -> f-33) (\g -> g+6) (\h -> h/5) (Right (Right (Right (Right (Right (Right (Right (Left 88))))))))
  log $ "either9 : forall r a b c d e f g h i. (a -> r) -> (b -> r) -> (c -> r) -> (d -> r) -> (e -> r) -> (f -> r) -> (g -> r) -> (h -> r) -> (i -> r) -> Either9 a b c d e f g h i -> r"
  logShow $ either9 (\a -> a-6) (\b -> b+89) (\c -> c+55) (\d -> d/2) (\e -> e+45) (\f -> f-33) (\g -> g+6) (\h -> h/5) (\i -> i*i) (Right (Right (Right (Right (Right (Right (Right (Right (Left 88)))))))))
  log $ "either10 : forall r a b c d e f g h i j. (a -> r) -> (b -> r) -> (c -> r) -> (d -> r) -> (e -> r) -> (f -> r) -> (g -> r) -> (h -> r) -> (i -> r) -> (j -> r) -> Either10 a b c d e f g h i j -> r"
  logShow $ either10 (\a -> a-6) (\b -> b+89) (\c -> c+55) (\d -> d/2) (\e -> e+45) (\f -> f-33) (\g -> g+6) (\h -> h/5) (\i -> i*i) (\j -> j+j) (Right (Right (Right (Right (Right (Right (Right (Right (Right (Left 88))))))))))


