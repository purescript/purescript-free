module Teletype where

import Control.Monad.Eff
import Control.Monad.Free
import Data.Coyoneda
import Debug.Trace

type FreeC f a = Free (Coyoneda f) a

data TeletypeF a = PutStrLn String a | GetLine (String -> a)

type Teletype a = FreeC TeletypeF a

type TeletypeC a = Coyoneda TeletypeF a

putStrLn :: String -> Teletype Unit
putStrLn s = liftF $ liftCoyoneda $ PutStrLn s unit

getLine :: Teletype String
getLine = liftF $ liftCoyoneda $ GetLine id

teletypeNat :: forall e. Natural TeletypeF (Eff (trace :: Trace))
teletypeNat (PutStrLn s a) = (\_ -> a) <$> trace s
teletypeNat (GetLine k) = return $ k "fake input"

run :: forall a. Teletype a -> Eff (trace :: Trace) a
run = goEff (liftCoyonedaTF teletypeNat)

echo = do
  a <- getLine
  putStrLn a
  putStrLn "Finished"
  return $ a ++ a

main = do
  a <- run $ echo
  trace a
