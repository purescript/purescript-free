module Example.TeletypeCoproduct where

import Prelude

import Control.Alt ((<|>))
import Control.Apply ((*>))
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console
import Control.Monad.Free (FreeC(), liftFC, injC, runFreeCM)
import Data.Coyoneda (Natural())
import Data.Functor.Coproduct (Coproduct())
import Data.Inject (prj)
import Data.Maybe.Unsafe (fromJust)

data Teletype1F a = Print1 String a

type Teletype1 a = FreeC Teletype1F a

print1 :: String -> Teletype1 Unit
print1 a = liftFC $ Print1 a unit

data Teletype2F a = Print2 String a

type Teletype2 a = FreeC Teletype2F a

print2 :: String -> Teletype2 Unit
print2 a = liftFC $ Print2 a unit

data Teletype3F a = Print3 String a

type Teletype3 a = FreeC Teletype3F a

print3 :: String -> Teletype3 Unit
print3 a = liftFC $ Print3 a unit

type TF = Coproduct Teletype1F (Coproduct Teletype2F Teletype3F)

type T a = FreeC TF a

r :: T Unit
r = injC $ print1 "1"

s :: T Unit
s = injC $ print2 "2"

t :: T Unit
t = injC $ print3 "3"

u :: T Unit
u =  r *> s *> t

teletype1N :: forall e. Natural Teletype1F (Eff (console :: CONSOLE | e))
teletype1N (Print1 s a) = const a <$> log ("teletype1: " ++ s)

teletype2N :: forall e. Natural Teletype2F (Eff (console :: CONSOLE | e))
teletype2N (Print2 s a) = const a <$> log ("teletype2: " ++ s)

teletype3N :: forall e. Natural Teletype3F (Eff (console :: CONSOLE | e))
teletype3N (Print3 s a) = const a <$> log ("teletype3: " ++ s)

tN :: forall e. Natural TF (Eff (console :: CONSOLE | e))
tN fa = fromJust $ (teletype1N <$> prj fa) <|>
                   (teletype2N <$> prj fa) <|>
                   (teletype3N <$> prj fa)

run :: forall a. T a -> Eff (console :: CONSOLE) a
run = runFreeCM tN

main = run u
