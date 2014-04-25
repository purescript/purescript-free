module Control.Monad.Free where

import Prelude
import Control.Monad.Trans

data Free f a = Pure a | Free (f (Free f a))

class MonadFree f m where
  wrap :: forall a. f (m a) -> m a

instance functorFree :: (Functor f) => Functor (Free f) where
  (<$>) f = go where
    go (Pure a)  = Pure (f a)
    go (Free fa) = Free (go <$> fa)

instance applyFree :: (Functor f) => Apply (Free f) where
  (<*>) = ap

instance applicativeFree :: (Functor f) => Applicative (Free f) where
  pure = Pure

instance bindFree :: (Functor f) => Bind (Free f) where
  (>>=) (Pure a) f = f a
  (>>=) (Free m) f = Free ((<$>) (\a -> a >>= f) m)

instance monadFree :: (Functor f) => Monad (Free f)

instance monadTransFree :: MonadTrans Free where
  lift f = Free $ do
    a <- f
    return (Pure a)

instance monadFreeFree :: (Functor f) => MonadFree f (Free f) where
  wrap = Free

liftF :: forall f m a. (Functor f, Monad m, MonadFree f m) => f a -> m a
liftF fa = wrap $ return <$> fa

iterM :: forall f m a. (Functor f, Monad m) => (f (m a) -> m a) -> Free f a -> m a
iterM _ (Pure a) = return a
iterM k (Free f) = k $ (iterM k) <$> f
