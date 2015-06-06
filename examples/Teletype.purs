module Teletype where

import Prelude

import Control.Monad.Eff
import Control.Monad.Free
import Data.Coyoneda
import Console

data TeletypeF a = PutStrLn String a | GetLine (String -> a)

type Teletype a = FreeC TeletypeF a

putStrLn :: String -> Teletype Unit
putStrLn s = liftFC $ PutStrLn s unit

getLine :: Teletype String
getLine = liftFC $ GetLine id

teletypeN :: forall e. Natural TeletypeF (Eff (console :: CONSOLE))
teletypeN (PutStrLn s a) = const a <$> log s
teletypeN (GetLine k) = return $ k "fake input"

run :: forall a. Teletype a -> Eff (console :: CONSOLE) a
run = runFreeCM teletypeN

echo = do
  a <- getLine
  putStrLn a
  putStrLn "Finished"
  return $ a ++ a

main = do
  a <- run $ echo
  log a
