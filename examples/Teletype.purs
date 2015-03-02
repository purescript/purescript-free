module Teletype where

import Control.Monad.Eff
import Control.Monad.Free
import Data.Coyoneda
import Debug.Trace

data TeletypeF a = PutStrLn String a | GetLine (String -> a)

type Teletype a = FreeC TeletypeF a

putStrLn :: String -> Teletype Unit
putStrLn s = liftFC $ PutStrLn s unit

getLine :: Teletype String
getLine = liftFC $ GetLine id

teletypeN :: forall e. Natural TeletypeF (Eff (trace :: Trace))
teletypeN (PutStrLn s a) = const a <$> trace s
teletypeN (GetLine k) = return $ k "fake input"

run :: forall a. Teletype a -> Eff (trace :: Trace) a
run = runFreeCM teletypeN

echo = do
  a <- getLine
  putStrLn a
  putStrLn "Finished"
  return $ a ++ a

main = do
  a <- run $ echo
  trace a
