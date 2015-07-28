module Data.Coyoneda
  ( Coyoneda(..)
  , CoyonedaF(..)
  , coyoneda
  , liftCoyoneda
  , lowerCoyoneda
  , liftCoyonedaT
  , liftCoyonedaTF
  ) where

import Prelude

import Data.Exists
import Data.NaturalTransformation (NaturalTransformation())

import Control.Comonad
import Control.Extend
import Control.Monad.Trans

-- | `Coyoneda` is encoded as an existential type using `Data.Exists`.
-- |
-- | This type constructor encodes the contents of the existential package.
newtype CoyonedaF f a i = CoyonedaF { k :: i -> a, fi :: f i }

-- | The `Coyoneda` `Functor`.
-- |
-- | `Coyoneda f` is a `Functor` for any type constructor `f`. In fact,
-- | it is the _free_ `Functor` for `f`.
newtype Coyoneda f a = Coyoneda (Exists (CoyonedaF f a))

instance functorCoyoneda :: Functor (Coyoneda f) where
  map f (Coyoneda e) = runExists (\(CoyonedaF v) -> coyoneda (f <<< v.k) v.fi) e

instance applyCoyoneda :: (Apply f) => Apply (Coyoneda f) where
  apply f g = liftCoyoneda $ lowerCoyoneda f <*> lowerCoyoneda g

instance applicativeCoyoneda :: (Applicative f) => Applicative (Coyoneda f) where
  pure = liftCoyoneda <<< pure

instance bindCoyoneda :: (Bind f) => Bind (Coyoneda f) where
  bind (Coyoneda e) k = liftCoyoneda $ runExists (\(CoyonedaF v) -> v.fi >>= lowerCoyoneda <<< k <<< v.k) e

instance monadCoyoneda :: (Monad f) => Monad (Coyoneda f)

instance monadTransCoyoneda :: MonadTrans Coyoneda where
  lift = liftCoyoneda

instance extendCoyoneda :: (Extend w) => Extend (Coyoneda w) where
  extend f (Coyoneda e) = runExists (\(CoyonedaF w) -> liftCoyoneda $ f <<< coyoneda w.k <<= w.fi) e

instance comonadCoyoneda :: (Comonad w) => Comonad (Coyoneda w) where
  extract (Coyoneda e) = runExists (\(CoyonedaF w) -> w.k $ extract w.fi) e

-- | Construct a value of type `Coyoneda f a`.
coyoneda :: forall f a b. (a -> b) -> f a -> Coyoneda f b
coyoneda k fi = Coyoneda $ mkExists $ CoyonedaF { k: k, fi: fi }

-- | Lift a value described by the type constructor `f` to `Coyoneda f`.
liftCoyoneda :: forall f a. f a -> Coyoneda f a
liftCoyoneda fa = Coyoneda $ mkExists $ CoyonedaF { k: id, fi: fa }

-- | Lower a value of type `Yoneda f a` to the `Functor` `f`.
lowerCoyoneda :: forall f a. (Functor f) => Coyoneda f a -> f a
lowerCoyoneda (Coyoneda e) = runExists (\(CoyonedaF v) -> v.k <$> v.fi) e

liftCoyonedaT :: forall f g. NaturalTransformation f g -> NaturalTransformation (Coyoneda f) (Coyoneda g)
liftCoyonedaT nat (Coyoneda e) = runExists (\(CoyonedaF v) -> coyoneda v.k (nat v.fi)) e

liftCoyonedaTF :: forall f g. (Functor g) => NaturalTransformation f g -> NaturalTransformation (Coyoneda f) g
liftCoyonedaTF nat = lowerCoyoneda <<< liftCoyonedaT nat
