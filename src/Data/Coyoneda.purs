module Data.Coyoneda
  ( Coyoneda(..)
  , CoyonedaF
  , coyoneda
  , unCoyoneda
  , liftCoyoneda
  , lowerCoyoneda
  , hoistCoyoneda
  ) where

import Prelude

import Control.Comonad (class Comonad, extract)
import Control.Extend (class Extend, (<<=))
import Control.Monad.Trans.Class (class MonadTrans)

import Data.Eq (class Eq1, eq1)
import Data.Exists (Exists, runExists, mkExists)
import Data.Ord (class Ord1, compare1)

-- | `Coyoneda` is encoded as an existential type using `Data.Exists`.
-- |
-- | This type constructor encodes the contents of the existential package.
data CoyonedaF f a i = CoyonedaF (i -> a) (f i)

-- | The `Coyoneda` `Functor`.
-- |
-- | `Coyoneda f` is a `Functor` for any type constructor `f`. In fact,
-- | it is the _free_ `Functor` for `f`.
newtype Coyoneda f a = Coyoneda (Exists (CoyonedaF f a))

instance eqCoyoneda :: (Functor f, Eq1 f, Eq a) => Eq (Coyoneda f a) where
  eq x y = lowerCoyoneda x `eq1` lowerCoyoneda y

instance eq1Coyoneda :: (Functor f, Eq1 f) => Eq1 (Coyoneda f) where
  eq1 = eq

instance ordCoyoneda :: (Functor f, Ord1 f, Ord a) => Ord (Coyoneda f a) where
  compare x y = lowerCoyoneda x `compare1` lowerCoyoneda y

instance ord1Coyoneda :: (Functor f, Ord1 f) => Ord1 (Coyoneda f) where
  compare1 = compare

instance functorCoyoneda :: Functor (Coyoneda f) where
  map f (Coyoneda e) = runExists (\(CoyonedaF k fi) -> coyoneda (f <<< k) fi) e

instance applyCoyoneda :: Apply f => Apply (Coyoneda f) where
  apply f g = liftCoyoneda $ lowerCoyoneda f <*> lowerCoyoneda g

instance applicativeCoyoneda :: Applicative f => Applicative (Coyoneda f) where
  pure = liftCoyoneda <<< pure

instance bindCoyoneda :: Bind f => Bind (Coyoneda f) where
  bind (Coyoneda e) f =
    liftCoyoneda $
      runExists (\(CoyonedaF k fi) -> lowerCoyoneda <<< f <<< k =<< fi) e

instance monadCoyoneda :: Monad f => Monad (Coyoneda f)

instance monadTransCoyoneda :: MonadTrans Coyoneda where
  lift = liftCoyoneda

instance extendCoyoneda :: Extend w => Extend (Coyoneda w) where
  extend f (Coyoneda e) =
    runExists (\(CoyonedaF k fi) -> liftCoyoneda $ f <<< coyoneda k <<= fi) e

instance comonadCoyoneda :: Comonad w => Comonad (Coyoneda w) where
  extract (Coyoneda e) = runExists (\(CoyonedaF k fi) -> k $ extract fi) e

-- | Construct a value of type `Coyoneda f b` from a mapping function and a
-- | value of type `f a`.
coyoneda :: forall f a b. (a -> b) -> f a -> Coyoneda f b
coyoneda k fi = Coyoneda $ mkExists $ CoyonedaF k fi

-- | Deconstruct a value of `Coyoneda a` to retrieve the mapping function and
-- | original value.
unCoyoneda :: forall f g a. (forall b. (b -> a) -> f b -> g a) -> Coyoneda f a -> g a
unCoyoneda f (Coyoneda e) = runExists (\(CoyonedaF k fi) -> f k fi) e

-- | Lift a value described by the type constructor `f` to `Coyoneda f`.
liftCoyoneda :: forall f. f ~> Coyoneda f
liftCoyoneda = coyoneda id

-- | Lower a value of type `Coyoneda f a` to the `Functor` `f`.
lowerCoyoneda :: forall f. Functor f => Coyoneda f ~> f
lowerCoyoneda = unCoyoneda map

-- | Use a natural transformation to change the generating type constructor of a
-- | `Coyoneda`.
hoistCoyoneda :: forall f g. (f ~> g) -> Coyoneda f ~> Coyoneda g
hoistCoyoneda nat (Coyoneda e) =
  runExists (\(CoyonedaF k fi) -> coyoneda k (nat fi)) e
