module Control.Monad.Trampoline where

import Control.Monad.Free

newtype Delay a = Delay (Unit -> a)

instance delayFunctor :: Functor Delay where
  (<$>) f (Delay g) = Delay (const (f (g unit)))

instance delayApply :: Apply Delay where
  (<*>) (Delay f) (Delay a) = Delay (\_ -> (f unit) (a unit))

instance delayApplicative :: Applicative Delay where
  pure a = Delay (const a)

type Trampoline a = Free Delay a

done :: forall a. a -> Trampoline a
done = Pure

suspend :: forall a. Trampoline a -> Trampoline a
suspend a = Free (Delay (const a))

delay :: forall a. (Unit -> a) -> Trampoline a
delay a = Free (done <$> Delay a)

runTrampoline :: forall a. Trampoline a -> a
runTrampoline = go (\(Delay f) -> f unit)
