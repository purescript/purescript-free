module Test.Control.Monad.Free.Teletype where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Free (Free, foldFree, liftF)

import Data.NaturalTransformation (NaturalTransformation)

data TeletypeF a = PutStrLn String a | GetLine (String -> a)

type Teletype a = Free TeletypeF a

putStrLn :: String -> Teletype Unit
putStrLn s = liftF (PutStrLn s unit)

getLine :: Teletype String
getLine = liftF (GetLine id)

teletypeN :: forall eff. NaturalTransformation TeletypeF (Eff (console :: CONSOLE | eff))
teletypeN (PutStrLn s a) = const a <$> log s
teletypeN (GetLine k) = pure (k "fake input")

run :: forall eff. NaturalTransformation Teletype (Eff (console :: CONSOLE | eff))
run = foldFree teletypeN

echo :: Teletype String
echo = do
  a <- getLine
  putStrLn a
  putStrLn "Finished"
  pure $ a <> a

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = do
  a <- run $ echo
  log a
