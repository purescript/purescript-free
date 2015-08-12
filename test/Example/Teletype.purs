module Example.Teletype where

import Prelude

import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Free

import Data.NaturalTransformation

data TeletypeF a = PutStrLn String a | GetLine (String -> a)

type Teletype a = Free TeletypeF a

putStrLn :: String -> Teletype Unit
putStrLn s = liftF (PutStrLn s unit)

getLine :: Teletype String
getLine = liftF (GetLine id)

teletypeN :: forall e. NaturalTransformation TeletypeF (Eff (console :: CONSOLE))
teletypeN (PutStrLn s a) = const a <$> log s
teletypeN (GetLine k) = return (k "fake input")

run :: forall a. Teletype a -> Eff (console :: CONSOLE) a
run = foldFree teletypeN

echo = do
  a <- getLine
  putStrLn a
  putStrLn "Finished"
  return $ a ++ a

main = do
  a <- run $ echo
  log a
