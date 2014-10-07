module Control.Monad.Trampoline
  ( Trampoline()
  , done
  , suspend
  , delay'
  , delay
  , runTrampoline
  ) where

import Control.Monad.Free

import Data.Lazy
import Data.Foldable 
import Data.Traversable

type Trampoline = Free Lazy

done :: forall a. a -> Trampoline a
done = pure

suspend :: forall a. Trampoline a -> Trampoline a
suspend t = Free (defer (const t))

delay' :: forall a. Lazy a -> Trampoline a
delay' a = Free (done <$> a)

delay :: forall a. (Unit -> a) -> Trampoline a
delay = delay' <<< defer

runTrampoline :: forall a. Trampoline a -> a
runTrampoline = go force
