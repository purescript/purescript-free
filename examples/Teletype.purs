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

runF :: forall a. TeletypeF a -> Eff (trace :: Trace) a
runF (PutStrLn s a) = (\_ -> a) <$> trace s
runF (GetLine k) = return $ k "fake input"

run :: forall a. Teletype a -> Eff (trace :: Trace) a
run = goEff runF

echo = do
  a <- getLine
  putStrLn a
  putStrLn "Finished"

main = run $ echo
