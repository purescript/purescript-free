module Test.Control.Monad.Free.Coproduct where

import Prelude

import Control.Monad.Free (Free, hoist, interpret, lift)
import Data.Functor.Coproduct (Coproduct, coproduct, left, right)
import Effect (Effect)
import Effect.Console (log)

data Teletype1F a = Print1 String a

type Teletype1 a = Free Teletype1F a

print1 :: String -> Teletype1 Unit
print1 a = lift (Print1 a unit)

data Teletype2F a = Print2 String a

type Teletype2 a = Free Teletype2F a

print2 :: String -> Teletype2 Unit
print2 a = lift (Print2 a unit)

data Teletype3F a = Print3 String a

type Teletype3 a = Free Teletype3F a

print3 :: String -> Teletype3 Unit
print3 a = lift (Print3 a unit)

type TF = Coproduct Teletype1F (Coproduct Teletype2F Teletype3F)

type T a = Free TF a

r :: T Unit
r = hoist left (print1 "1")

s :: T Unit
s = hoist (right <<< left) (print2 "2")

t :: T Unit
t = hoist (right <<< right) (print3 "3")

u :: T Unit
u =  r *> s *> t

teletype1N :: Teletype1F ~> Effect
teletype1N (Print1 x a) = const a <$> log ("teletype1: " <> x)

teletype2N :: Teletype2F ~> Effect
teletype2N (Print2 x a) = const a <$> log ("teletype2: " <> x)

teletype3N :: Teletype3F ~> Effect
teletype3N (Print3 x a) = const a <$> log ("teletype3: " <> x)

tN :: TF ~> Effect
tN = coproduct teletype1N $ coproduct teletype2N teletype3N

run :: T ~> Effect
run = interpret tN

main :: Effect Unit
main = run u
