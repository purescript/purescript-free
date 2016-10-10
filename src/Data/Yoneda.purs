module Data.Yoneda
  ( Yoneda(..)
  , runYoneda
  , liftYoneda
  , lowerYoneda
  , hoistYoneda
  ) where

import Prelude

import Control.Comonad (class Comonad, extract)
import Control.Extend (class Extend, (<<=))
import Control.Monad.Trans.Class (class MonadTrans)

-- | The Yoneda `Functor`
-- |
-- | `Yoneda f` is a `Functor` for any type constructor `f`.
newtype Yoneda f a = Yoneda (forall b. (a -> b) -> f b)

instance functorYoneda :: Functor (Yoneda f) where
  map f m = Yoneda (\k -> runYoneda m (k <<< f))

instance applyYoneda :: Apply f => Apply (Yoneda f) where
  apply (Yoneda f) (Yoneda g) = Yoneda (\k -> f (compose k) <*> g id)

instance applicativeYoneda :: Applicative f => Applicative (Yoneda f) where
  pure = liftYoneda <<< pure

instance bindYoneda :: Bind f => Bind (Yoneda f) where
  bind (Yoneda f) g = Yoneda (\k -> f id >>= \a -> runYoneda (g a) k)

instance monadYoneda :: Monad f => Monad (Yoneda f)

instance monadTransYoneda :: MonadTrans Yoneda where
  lift = liftYoneda

instance extendYoneda :: Extend w => Extend (Yoneda w) where
  extend f (Yoneda w) = Yoneda (\k -> k <<< f <<< liftYoneda <<= w id)

instance comonadYoneda :: Comonad w => Comonad (Yoneda w) where
  extract = extract <<< lowerYoneda

-- | Run a computation of type `Yoneda f a`.
runYoneda :: forall f a b. Yoneda f a -> (a -> b) -> f b
runYoneda (Yoneda f) k = f k

-- | Lift a value described by the `Functor` `f` to the `Functor` `Yoneda f`.
liftYoneda :: forall f a. Functor f => f a -> Yoneda f a
liftYoneda m = Yoneda (\k -> k <$> m)

-- | Lower a value of type `Yoneda f a` to the type constructor `f`.
lowerYoneda :: forall f a. Yoneda f a -> f a
lowerYoneda (Yoneda k) = k id

-- | Use a natural transformation to change the generating type constructor of a
-- | `Yoneda`.
hoistYoneda :: forall f g a. (f ~> g) -> Yoneda f a -> Yoneda g a
hoistYoneda nat (Yoneda k) = Yoneda (nat <$> k)
