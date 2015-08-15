module Test.Control.Monad.Free.Coproduct where

import Prelude

import Control.Alt ((<|>))
import Control.Apply ((*>))
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console
import Control.Monad.Free (Free(), liftF, foldFree, injF)

import Data.Functor.Coproduct (Coproduct())
import Data.Inject (prj)
import Data.Maybe.Unsafe (fromJust)
import Data.NaturalTransformation (NaturalTransformation())

data Teletype1F a = Print1 String a

type Teletype1 a = Free Teletype1F a

print1 :: String -> Teletype1 Unit
print1 a = liftF (Print1 a unit)

data Teletype2F a = Print2 String a

type Teletype2 a = Free Teletype2F a

print2 :: String -> Teletype2 Unit
print2 a = liftF (Print2 a unit)

data Teletype3F a = Print3 String a

type Teletype3 a = Free Teletype3F a

print3 :: String -> Teletype3 Unit
print3 a = liftF (Print3 a unit)

type TF = Coproduct Teletype1F (Coproduct Teletype2F Teletype3F)

type T a = Free TF a

r :: T Unit
r = injF (print1 "1")

s :: T Unit
s = injF (print2 "2")

t :: T Unit
t = injF (print3 "3")

u :: T Unit
u =  r *> s *> t

teletype1N :: forall e. NaturalTransformation Teletype1F (Eff (console :: CONSOLE | e))
teletype1N (Print1 x a) = const a <$> log ("teletype1: " ++ x)

teletype2N :: forall e. NaturalTransformation Teletype2F (Eff (console :: CONSOLE | e))
teletype2N (Print2 x a) = const a <$> log ("teletype2: " ++ x)

teletype3N :: forall e. NaturalTransformation Teletype3F (Eff (console :: CONSOLE | e))
teletype3N (Print3 x a) = const a <$> log ("teletype3: " ++ x)

tN :: forall e. NaturalTransformation TF (Eff (console :: CONSOLE | e))
tN fa = fromJust $ (teletype1N <$> prj fa) <|>
                   (teletype2N <$> prj fa) <|>
                   (teletype3N <$> prj fa)

run :: forall a. T a -> Eff (console :: CONSOLE) a
run = foldFree tN

main = run u
