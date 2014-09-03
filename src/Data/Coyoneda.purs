module Data.Coyoneda
  ( Coyoneda(..)
  , CoyonedaF(..)
  , Natural(..)
  , coyoneda
  , liftCoyoneda
  , lowerCoyoneda
  , liftCoyonedaT
  , liftCoyonedaTF
  ) where

import Data.Exists

import Control.Comonad
import Control.Extend
import Control.Monad.Trans

newtype CoyonedaF f a i = CoyonedaF { k :: i -> a, fi :: f i }

newtype Coyoneda f a = Coyoneda (Exists (CoyonedaF f a))

type Natural f g = forall a. f a -> g a

instance functorCoyoneda :: Functor (Coyoneda f) where
  (<$>) f (Coyoneda e) = runExists (\(CoyonedaF v) -> coyoneda (f <<< v.k) v.fi) e

instance applyCoyoneda :: (Apply f) => Apply (Coyoneda f) where
  (<*>) f g = liftCoyoneda $ lowerCoyoneda f <*> lowerCoyoneda g

instance applicativeCoyoneda :: (Applicative f) => Applicative (Coyoneda f) where
  pure = liftCoyoneda <<< pure

instance bindCoyoneda :: (Bind f) => Bind (Coyoneda f) where
  (>>=) (Coyoneda e) k = liftCoyoneda $ runExists (\(CoyonedaF v) -> v.fi >>= lowerCoyoneda <<< k <<< v.k) e

instance monadCoyoneda :: (Monad f) => Monad (Coyoneda f)

instance monadTransCoyoneda :: MonadTrans Coyoneda where
  lift = liftCoyoneda

instance extendCoyoneda :: (Extend w) => Extend (Coyoneda w) where
  (<<=) f (Coyoneda e) = runExists (\(CoyonedaF w) -> liftCoyoneda $ f <<< coyoneda w.k <<= w.fi) e

instance comonadCoyoneda :: (Comonad w) => Comonad (Coyoneda w) where
  extract (Coyoneda e) = runExists (\(CoyonedaF w) -> w.k $ extract w.fi) e

coyoneda :: forall f a b. (a -> b) -> f a -> Coyoneda f b
coyoneda k fi = Coyoneda $ mkExists $ CoyonedaF { k: k, fi: fi }

liftCoyoneda :: forall f a. f a -> Coyoneda f a
liftCoyoneda fa = Coyoneda $ mkExists $ CoyonedaF { k: id, fi: fa }

lowerCoyoneda :: forall f a. (Functor f) => Coyoneda f a -> f a
lowerCoyoneda (Coyoneda e) = runExists (\(CoyonedaF v) -> v.k <$> v.fi) e

liftCoyonedaT :: forall f g. Natural f g -> Natural (Coyoneda f) (Coyoneda g)
liftCoyonedaT nat (Coyoneda e) = runExists (\(CoyonedaF v) -> coyoneda v.k (nat v.fi)) e

liftCoyonedaTF :: forall f g. (Functor g) => Natural f g -> Natural (Coyoneda f) g
liftCoyonedaTF nat = lowerCoyoneda <<< liftCoyonedaT nat
