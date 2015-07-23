module Example.ReinterpretWithBindF where

import Prelude

import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Free
import Data.Coyoneda
import Data.Functor (($>))

-- | Target DSL that we will actually run
data TeletypeF a
  = PutStrLn String a
  | GetLine (String -> a)

type Teletype = FreeC TeletypeF

putStrLn :: String -> Teletype Unit
putStrLn s = liftFC $ PutStrLn s unit

getLine :: Teletype String
getLine = liftFC $ GetLine id

-- | Interpreter for `Teletype`, producing an effectful output
runTeletype :: forall a. Teletype a -> Eff (console :: CONSOLE) a
runTeletype = runFreeCM go
  where
  go :: Natural TeletypeF (Eff (console :: CONSOLE))
  go (PutStrLn s next) = log s $> next
  go (GetLine k) = pure (k "fake input")

-- | Initial DSL that we will reinterpret as TeletypeF
data InitialF a
  = Greet (String -> a)
  | Farewell a

type Initial = FreeC InitialF

greet :: Initial String
greet = liftFC $ Greet id

farewell :: Initial Unit
farewell = liftFC $ Farewell unit

-- | Interpreter for `Initial`, producing a `Teletype` output. `bindF` allows
-- | us to map one action in `InitialF` to multiple actions in `TeletypeF` (see
-- | the `Greet` case - we're expanding one `InitialF` action into 3 `TeletypeF`
-- | actions).
runInitial :: forall a. Initial a -> Teletype a
runInitial initial = bindFC initial go
  where
  go :: Natural InitialF Teletype
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
