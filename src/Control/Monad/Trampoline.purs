module Control.Monad.Trampoline where

import Control.Monad.Free

data Delay a = Delay ({} -> a)

instance delayFunctor :: Functor Delay where
  (<$>) f (Delay g) = Delay (const (f (g {})))

instance delayApply :: Apply Delay where
  (<*>) (Delay f) (Delay a) = Delay (\{} -> (f {}) (a {}))

instance delayApplicative :: Applicative Delay where
  pure a = Delay (\{} -> a)

type Trampoline a = Free Delay a

done :: forall a. a -> Trampoline a
done = Pure

suspend :: forall a. Trampoline a -> Trampoline a
suspend a = Free (Delay (\{} -> a))

delay :: forall a. ({} -> a) -> Trampoline a
delay a = Free (done <$> Delay a)

runTrampoline :: forall a. Trampoline a -> a
runTrampoline = go (\(Delay f) -> f {})
