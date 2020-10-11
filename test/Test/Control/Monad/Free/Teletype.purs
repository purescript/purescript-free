module Test.Control.Monad.Free.Teletype where

import Prelude

import Control.Monad.Free (Free, interpret, lift)
import Effect (Effect)
import Effect.Console (log)

data TeletypeF a = PutStrLn String a | GetLine (String -> a)

type Teletype a = Free TeletypeF a

putStrLn :: String -> Teletype Unit
putStrLn s = lift (PutStrLn s unit)

getLine :: Teletype String
getLine = lift (GetLine identity)

teletypeN :: TeletypeF ~> Effect
teletypeN (PutStrLn s a) = const a <$> log s
teletypeN (GetLine k) = pure (k "fake input")

run :: Teletype ~> Effect
run = interpret teletypeN

echo :: Teletype String
echo = do
  a <- getLine
  putStrLn a
  putStrLn "Finished"
  pure $ a <> a

main :: Effect Unit
main = do
  a <- run $ echo
  log a
