module Test.Control.Monad.Free.Teletype where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Control.Monad.Free (Free, foldFree, liftF)

data TeletypeF a = PutStrLn String a | GetLine (String -> a)

type Teletype a = Free TeletypeF a

putStrLn :: String -> Teletype Unit
putStrLn s = liftF (PutStrLn s unit)

getLine :: Teletype String
getLine = liftF (GetLine identity)

teletypeN :: TeletypeF ~> Effect
teletypeN (PutStrLn s a) = const a <$> log s
teletypeN (GetLine k) = pure (k "fake input")

run :: Teletype ~> Effect
run = foldFree teletypeN

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
