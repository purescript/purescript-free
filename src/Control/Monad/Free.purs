module Control.Monad.Free
  ( Free(..)
  , FreeC(..)
  , MonadFree, wrap
  , liftF, liftFC
  , pureF, pureFC
  , mapF, injC
  , iterM
  , goM, goMC
  , go
  , goEff, goEffC
  ) where

import Control.Monad.Trans
import Control.Monad.Eff
import Data.Coyoneda
import Data.Either
import Data.Function
import Data.Inject (Inject, inj)

data Free f a = Pure a
              | Free (f (Free f a))
              | Gosub (forall s. (forall r. (Unit -> Free f r) -> (r -> Free f a) -> s) -> s)

type FreeC f = Free (Coyoneda f)

class MonadFree f m where
  wrap :: forall a. f (m a) -> m a

instance functorFree :: (Functor f) => Functor (Free f) where
  (<$>) f (Pure a) = Pure (f a)
  (<$>) f g = liftA1 f g

instance applyFree :: (Functor f) => Apply (Free f) where
  (<*>) = ap

instance applicativeFree :: (Functor f) => Applicative (Free f) where
  pure = Pure

instance bindFree :: (Functor f) => Bind (Free f) where
  (>>=) (Gosub g) f = Gosub (\h -> g (\a i -> h a (\x -> Gosub (\j -> j (const (i x)) f))))
  (>>=) a         f = Gosub (\h -> h (const a) f)

instance monadFree :: (Functor f) => Monad (Free f)

instance monadTransFree :: MonadTrans Free where
  lift f = Free $ do
    a <- f
    return (Pure a)

instance monadFreeFree :: (Functor f) => MonadFree f (Free f) where
  wrap = Free

liftF :: forall f m a. (Functor f, Monad m, MonadFree f m) => f a -> m a
liftF = wrap <<< (<$>) return

pureF :: forall f a. (Applicative f) => a -> Free f a
pureF = Free <<< pure <<< Pure

liftFC :: forall f a. f a -> FreeC f a
liftFC = liftF <<< liftCoyoneda

pureFC :: forall f a. (Applicative f) => a -> FreeC f a
pureFC = liftFC <<< pure

mapF :: forall f g a. (Functor f, Functor g) => Natural f g -> Free f a -> Free g a
mapF t fa = either (\s -> Free <<< t $ mapF t <$> s) Pure (resume fa)

injC :: forall f g a. (Inject f g) => FreeC f a -> FreeC g a
injC = mapF (liftCoyonedaT inj)

-- Note: can blow the stack!
iterM :: forall f m a. (Functor f, Monad m) => (forall a. f (m a) -> m a) -> Free f a -> m a
iterM _ (Pure a) = return a
iterM k (Free f) = k $ iterM k <$> f
iterM k (Gosub f) = f (\req recv -> iterM k (req unit) >>= (iterM k <<< recv))

-- Note: can blow the stack!
goM :: forall f m a. (Functor f, Monad m) => (f (Free f a) -> m (Free f a)) -> Free f a -> m a
goM k f = case resume f of
            Left s -> k s >>= goM k
            Right a -> return a

resumeGosub :: forall f a. (Functor f) => Free f a -> Either (f (Free f a)) (Free f a)
resumeGosub (Gosub f) = f (\a g ->
  case a unit of
    Pure a -> Right (g a)
    Free t -> Left ((\h -> h >>= g) <$> t)
    Gosub h -> Right (h (\b i -> b unit >>= (\x -> i x >>= g)))
  )

unsafeLeft :: forall a b. Either a b -> a
unsafeLeft (Left x) = x

unsafeRight :: forall a b. Either a b -> b
unsafeRight (Right x) = x

resume :: forall f a. (Functor f) => Free f a -> Either (f (Free f a)) a
resume f = case f of
  Pure x -> Right x
  Free x -> Left x
  g -> case resumeGosub g of
    Left l -> Left l
    Right r -> resume r

go :: forall f a. (Functor f) => (f (Free f a) -> Free f a) -> Free f a -> a
go fn f = case resume f of
  Left l -> go fn (fn l)
  Right r -> r

foreign import goEffImpl """
  function goEffImpl(resume, isRight, fromLeft, fromRight, fn, value) {
    return function(){
      while (true) {
        var r = resume(value);
        if (isRight(r)) return fromRight(r);
        value = fn(fromLeft(r))();
      }
    };
  }""" :: forall e f a. Fn6
          (Free f a -> Either (f (Free f a)) a)
          (Either (f (Free f a)) a -> Boolean)
          (Either (f (Free f a)) a -> (f (Free f a)))
          (Either (f (Free f a)) a -> a)
          (f (Free f a) -> Eff e (Free f a))
          (Free f a)
          (Eff e a)

goEff :: forall e f a. (Functor f) => (f (Free f a) -> Eff e (Free f a)) -> Free f a -> Eff e a
goEff fn f = runFn6 goEffImpl resume isRight unsafeLeft unsafeRight fn f

-- Note: can blow the stack!
goMC :: forall f m a. (Monad m) => Natural f m -> FreeC f a -> m a
goMC nat = goM (liftCoyonedaTF nat)

goEffC :: forall e f a. Natural f (Eff e) -> FreeC f a -> Eff e a
goEffC nat = goEff (liftCoyonedaTF nat)
