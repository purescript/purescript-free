module Control.Monad.Trampoline where

import Control.Monad.Free
import Data.Lazy

type Trampoline a = Free Lazy a

done :: forall a. a -> Trampoline a
done = Pure

suspend :: forall a. Trampoline a -> Trampoline a
suspend a = Free (defer (const a))

delay :: forall a. (Unit -> a) -> Trampoline a
delay a = Free (done <$> defer a)

runTrampoline :: forall a. Trampoline a -> a
runTrampoline = go force
