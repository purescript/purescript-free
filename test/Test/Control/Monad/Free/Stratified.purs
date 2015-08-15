module Test.Control.Monad.Free.Stratified where

import Prelude

import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Free

import Data.Functor (($>))
import Data.NaturalTransformation

-- | Target DSL that we will actually run
data TeletypeF a
  = PutStrLn String a
  | GetLine (String -> a)

type Teletype = Free TeletypeF

putStrLn :: String -> Teletype Unit
putStrLn s = liftF $ PutStrLn s unit

getLine :: Teletype String
getLine = liftF $ GetLine id

-- | Interpreter for `Teletype`, producing an effectful output
runTeletype :: forall a. Teletype a -> Eff (console :: CONSOLE) a
runTeletype = foldFree go
  where
  go :: NaturalTransformation TeletypeF (Eff (console :: CONSOLE))
  go (PutStrLn s next) = log s $> next
  go (GetLine k) = pure (k "fake input")

-- | Initial DSL that we will reinterpret as TeletypeF
data InitialF a
  = Greet (String -> a)
  | Farewell a

type Initial = Free InitialF

greet :: Initial String
greet = liftF $ Greet id

farewell :: Initial Unit
farewell = liftF $ Farewell unit

-- | Interpreter for `Initial`, producing a `Teletype` output. `foldFree` allows
-- | us to map one action in `InitialF` to multiple actions in `TeletypeF` (see
-- | the `Greet` case - we're expanding one `InitialF` action into 3 `TeletypeF`
-- | actions).
runInitial :: forall a. Initial a -> Teletype a
runInitial initial = foldFree go initial
  where
  go :: NaturalTransformation InitialF Teletype
  go (Greet k) = do
    name <- getLine
    putStrLn $ "Hello " ++ name
    putStrLn "How's things?"
    pure (k name)
  go (Farewell next) = putStrLn "Bye!" $> next

-- | Our test "script" in the Initial DSL
test :: Initial String
test = do
  name <- greet
  farewell
  pure name

-- Run the thing
main = do
  a <- runTeletype (runInitial test)
  log $ "Input name while running: " ++ a
