-- | Free applicatives in the style of Ørjan Johansen.
module Control.Applicative.Free
  ( FreeAp(..)
  , ApF(..)
  , Ap
  , mkAp
  , unAp
  , liftAp
  , lowerAp
  , hoistAp
  ) where

import Prelude

import Unsafe.Coerce (unsafeCoerce)

data FreeAp f a
  = Pure a
  | Ap (Ap f a)

data ApF f a b = ApF (FreeAp f (a -> b)) (f a)
data Ap (f :: * -> *) a

mkAp :: forall f a b. ApF f a b -> Ap f b
mkAp = unsafeCoerce

unAp :: forall f b r. (forall a. ApF f a b -> r) -> Ap f b -> r
unAp = unsafeCoerce

instance functorFreeAp :: Functor (FreeAp f) where
  map f (Pure a) = Pure (f a)
  map f (Ap e) = unAp (\(ApF x y) -> Ap $ mkAp $ ApF ((f <<< _) <$> x) y) e

instance applyFreeAp :: Apply (FreeAp f) where
  apply (Pure f) y = map f y
  apply (Ap e) z = unAp (\(ApF x y) -> Ap $ mkAp $ ApF (flip <$> x <*> z) y) e

instance applicativeFreeAp :: Applicative (FreeAp f) where
  pure = Pure

liftAp :: forall f a. f a -> FreeAp f a
liftAp = Ap <<< mkAp <<< ApF (Pure id)

hoistAp :: forall f g a. (f ~> g) -> FreeAp f a -> FreeAp g a
hoistAp _ (Pure a) = Pure a
hoistAp nat (Ap e) = unAp (\(ApF x y) -> Ap $ mkAp $ ApF (hoistAp nat x) (nat y)) e

lowerAp :: forall f a. Applicative f => FreeAp f a -> f a
lowerAp (Pure a) = pure a
lowerAp (Ap e) = unAp (\(ApF x y) -> lowerAp x <*> y) e
