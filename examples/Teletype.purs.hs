module Teletype where

import Control.Monad.Eff
import Control.Monad.Free
import Debug.Trace

data TeletypeF a = PutStrLn String a | GetLine (String -> a)

instance teletypeFFunctor :: Functor TeletypeF where
  (<$>) f (PutStrLn s a) = PutStrLn s (f a)
  (<$>) f (GetLine k) = GetLine (\s -> f (k s))

type Teletype = Free TeletypeF

putStrLn :: String -> Teletype {}
putStrLn s = liftF $ PutStrLn s {}

getLine :: Teletype String
getLine = liftF $ GetLine (\a -> a)

run :: forall e a. Teletype a -> Eff (trace :: Trace | e) a
run (Pure a) = return a
run (Free (PutStrLn s a)) = trace s >>= (\_ -> run a)
run (Free (GetLine k)) = run $ k "fake input"

echo :: Teletype {}
echo = do
  a <- getLine
  putStrLn a
  putStrLn "Finished"

main = run echo
