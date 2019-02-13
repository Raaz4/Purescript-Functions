module Main where

import Prelude (Unit, ($), discard, (*), (-), (+), (/))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Data.Either.Nested (type (\/), Either1, Either10, Either2, Either3, Either4, Either5, Either6, Either7, Either8, Either9, at1, at10, at2, at3, at4, at5, at6, at7, at8, at9, either1, either10, either2, either3, either4, either5, either6, either7, either8, either9, in1, in10, in2, in3, in4, in5, in6, in7, in8, in9)
import Data.Either (Either(..))

in11 :: forall a z. a -> a \/ z
in11 a = in1 a

in21 :: forall a b z. b -> a \/ b \/ z
in21 a = in2 a

in31 :: forall a b c z. c -> a \/ b \/ c \/ z
in31 a = in3 a

in41 :: forall a b c d z. d -> a \/ b \/ c \/ d \/ z
in41 a = in4 a

in51 :: forall a b c d e z. e -> a \/ b \/ c \/ d \/ e \/ z
in51 a = in5 a

in61 :: forall a b c d e f z. f -> a \/ b \/ c \/ d \/ e \/ f \/ z
in61 a = in6 a

in71 :: forall a b c d e f g z. g -> a \/ b \/ c \/ d \/ e \/ f \/ g \/ z
in71 a = in7 a

in81 :: forall a b c d e f g h z. h -> a \/ b \/ c \/ d \/ e \/ f \/ g \/ h \/ z
in81 a = in8 a

in91 :: forall a b c d e f g h i z. i -> a \/ b \/ c \/ d \/ e \/ f \/ g \/ h \/ i \/ z
in91 a = in9 a

in101 :: forall a b c d e f g h i j z. j -> a \/ b \/ c \/ d \/ e \/ f \/ g \/ h \/ i \/ j \/ z
in101 a = in10 a

at11 :: forall r a z. r -> (a -> r) -> a \/ z -> r
at11 r f e = at1 r f e

at21 :: forall r a b z. r -> (b -> r) -> a \/ b \/ z -> r
at21 r f e = at2 r f e

at31 :: forall r a b c z. r -> (c -> r) -> a \/ b \/ c \/ z -> r
at31 r f e = at3 r f e

at41 :: forall r a b c d z. r -> (d -> r) -> a \/ b \/ c \/ d \/ z -> r
at41 r f e = at4 r f e

at51 :: forall r a b c d e z. r -> (e -> r) -> a \/ b \/ c \/ d \/ e \/ z -> r
at51 r f e = at5 r f e

at61 :: forall r a b c d e f z. r -> (f -> r) -> a \/ b \/ c \/ d \/ e \/ f \/ z -> r
at61 r f e = at6 r f e

at71 :: forall r a b c d e f g z. r -> (g -> r) -> a \/ b \/ c \/ d \/ e \/ f \/ g \/ z -> r
at71 r f e = at7 r f e

at81 :: forall r a b c d e f g h z. r -> (h -> r) -> a \/ b \/ c \/ d \/ e \/ f \/ g \/ h \/ z -> r
at81 r f e = at8 r f e

at91 :: forall r a b c d e f g h i z. r -> (i -> r) -> a \/ b \/ c \/ d \/ e \/ f \/ g \/ h \/ i \/ z -> r
at91 r f e = at9 r f e

at101 :: forall r a b c d e f g h i j z. r -> (j -> r) -> a \/ b \/ c \/ d \/ e \/ f \/ g \/ h \/ i \/ j \/ z -> r
at101 r f e = at10 r f e

either11 :: forall a. Either1 a -> a
either11 e = either1 e

either21 :: forall r a b. (a -> r) -> (b -> r) -> Either2 a b -> r
either21 a b e = either2 a b e

either31 :: forall r a b c. (a -> r) -> (b -> r) -> (c -> r) -> Either3 a b c -> r
either31 a b c e = either3 a b c e

either41 :: forall r a b c d. (a -> r) -> (b -> r) -> (c -> r) -> (d -> r) -> Either4 a b c d -> r
either41 a b c d e = either4 a b c d e

either51 :: forall r a b c d e. (a -> r) -> (b -> r) -> (c -> r) -> (d -> r) -> (e -> r) -> Either5 a b c d e -> r
either51 a b c e = either5 a b c e

either61 :: forall r a b c d e f. (a -> r) -> (b -> r) -> (c -> r) -> (d -> r) -> (e -> r) -> (f -> r) -> Either6 a b c d e f -> r
either61 a b c e = either6 a b c e

either71 :: forall r a b c d e f g. (a -> r) -> (b -> r) -> (c -> r) -> (d -> r) -> (e -> r) -> (f -> r) -> (g -> r) -> Either7 a b c d e f g -> r
either71 a b c e = either7 a b c e

either81 :: forall r a b c d e f g h. (a -> r) -> (b -> r) -> (c -> r) -> (d -> r) -> (e -> r) -> (f -> r) -> (g -> r) -> (h -> r) -> Either8 a b c d e f g h -> r
either81 a b c e = either8 a b c e

either91 :: forall r a b c d e f g h i. (a -> r) -> (b -> r) -> (c -> r) -> (d -> r) -> (e -> r) -> (f -> r) -> (g -> r) -> (h -> r) -> (i -> r) -> Either9 a b c d e f g h i -> r
either91 a b c e = either9 a b c e

either101 :: forall r a b c d e f g h i j. (a -> r) -> (b -> r) -> (c -> r) -> (d -> r) -> (e -> r) -> (f -> r) -> (g -> r) -> (h -> r) -> (i -> r) -> (j -> r) -> Either10 a b c d e f g h i j -> r
either101 a b c e = either10 a b c e

main :: Effect Unit
main = do
  logShow $ in11 5 :: Either _ Int
  logShow $ in21 4 :: Either Int (Either _ Int)
  logShow $ in31 5 :: Either Int (Either Int (Either _ Int))
  logShow $ in41 3 :: Either Int (Either Int (Either Int (Either _ Int)))
  logShow $ in51 9 :: Either Int (Either Int (Either Int (Either Int (Either _ Int))))
  logShow $ in61 3 :: Either Int (Either Int (Either Int (Either Int (Either Int (Either _ Int)))))
  logShow $ in71 3 :: Either Int (Either Int (Either Int (Either Int (Either Int (Either Int (Either _ Int))))))
  logShow $ in81 3 :: Either Int (Either Int (Either Int (Either Int (Either Int (Either Int (Either Int (Either _ Int)))))))
  logShow $ in91 3 :: Either Int (Either Int (Either Int (Either Int (Either Int (Either Int (Either Int (Either Int (Either _ Int))))))))
  logShow $ in101 3 :: Either Int (Either Int (Either Int (Either Int (Either Int (Either Int (Either Int (Either Int (Either Int (Either _ Int)))))))))
  logShow $ at11 61 (\a -> a*a) (Left 4)
  logShow $ at21 61 (\b -> b*b) (Right (Left 8))
  logShow $ at31 41 (\c -> c*c) (Right (Right (Left 9)))
  logShow $ at41 51 (\d -> d*d) (Right (Right (Right (Left 31))))
  logShow $ at51 71 (\e -> e*e) (Right (Right (Right (Right (Left 11)))))
  logShow $ at61 21 (\f -> f*f) (Right (Right (Right (Right (Right (Left 54))))))
  logShow $ at71 61 (\g -> g*g) (Right (Right (Right (Right (Right (Right (Left 81)))))))
  logShow $ at81 16 (\h -> h*h) (Right (Right (Right (Right (Right (Right (Right (Left 18))))))))
  logShow $ at91 60 (\i -> i*i) (Right (Right (Right (Right (Right (Right (Right (Right (Left 80)))))))))
  logShow $ at101 66 (\j -> j*j) (Right (Right (Right (Right (Right (Right (Right (Right (Right (Left 90))))))))))
  logShow $ either11 (Left 400)
  logShow $ either21 (\a -> a-6) (\b -> b+89) (Right (Left 88))
  logShow $ either31 (\a -> a-6) (\b -> b+89) (\c -> c+55) (Right (Right (Left 88)))
  logShow $ either41 (\a -> a-6) (\b -> b+89) (\c -> c+55) (\d -> d/2) (Right (Right (Right (Left 88))))
  logShow $ either51 (\a -> a-6) (\b -> b+89) (\c -> c+55) (\d -> d/2) (\e -> e+23) (Right (Right (Right (Right (Left 88)))))
  logShow $ either61 (\a -> a-6) (\b -> b+89) (\c -> c+55) (\d -> d/2) (\e -> e+45) (\f -> f-33) (Right (Right (Right (Right (Right (Left 88))))))
  logShow $ either71 (\a -> a-6) (\b -> b+89) (\c -> c+55) (\d -> d/2) (\e -> e+45) (\f -> f-33) (\g -> g-6) (Right (Right (Right (Right (Right (Right (Left 88)))))))
  logShow $ either81 (\a -> a-6) (\b -> b+89) (\c -> c+55) (\d -> d/2) (\e -> e+45) (\f -> f-33) (\g -> g+6) (\h -> h/5) (Right (Right (Right (Right (Right (Right (Right (Left 88))))))))
  logShow $ either91 (\a -> a-6) (\b -> b+89) (\c -> c+55) (\d -> d/2) (\e -> e+45) (\f -> f-33) (\g -> g+6) (\h -> h/5) (\i -> i*i) (Right (Right (Right (Right (Right (Right (Right (Right (Left 88)))))))))
  logShow $ either101 (\a -> a-6) (\b -> b+89) (\c -> c+55) (\d -> d/2) (\e -> e+45) (\f -> f-33) (\g -> g+6) (\h -> h/5) (\i -> i*i) (\j -> j+j) (Right (Right (Right (Right (Right (Right (Right (Right (Right (Left 88))))))))))


