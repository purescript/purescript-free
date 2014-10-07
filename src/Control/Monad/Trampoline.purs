module Control.Monad.Trampoline
  ( Trampoline(..)
  , unTrampoline
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

newtype Trampoline a = Trampoline (Free Lazy a)

unTrampoline :: forall a. Trampoline a -> Free Lazy a
unTrampoline (Trampoline t) = t

done :: forall a. a -> Trampoline a
done a = Trampoline $ pure a

suspend :: forall a. Trampoline a -> Trampoline a
suspend = Trampoline <<< Free <<< defer <<< const <<< unTrampoline

delay' :: forall a. Lazy a -> Trampoline a
delay' = Trampoline <<< Free <<< (<$>) (unTrampoline <<< done)

delay :: forall a. (Unit -> a) -> Trampoline a
delay = delay' <<< defer

runTrampoline :: forall a. Trampoline a -> a
runTrampoline (Trampoline t) = go force t

instance functorTrampoline :: Functor Trampoline where
  (<$>) f (Trampoline t) = Trampoline $ f <$> t

instance applyTrampoline :: Apply Trampoline where
  (<*>) (Trampoline f) (Trampoline x) = Trampoline $ f <*> x

instance applicativeTrampoline :: Applicative Trampoline where
  pure a = Trampoline $ pure a

instance bindTrampoline :: Bind Trampoline where
  (>>=) (Trampoline ma) f = Trampoline $ ma >>= (unTrampoline <<< f)

instance monadTrampoline :: Monad Trampoline