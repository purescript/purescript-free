module Data.Inject
  ( Inject
  , inj, prj
  , injC
  ) where

import Control.Monad.Free (FreeC(), mapF)
import Data.Coproduct (Coproduct(..), coproduct)
import Data.Coyoneda (liftCoyonedaT)
import Data.Maybe (Maybe(..))

class Inject f g where
  inj :: forall a. f a -> g a
  prj :: forall a. g a -> Maybe (f a)

instance injectReflexive :: Inject f f where
  inj = id
  prj = Just

instance injectLeft :: Inject f (Coproduct f g) where
  inj = Inl
  prj = coproduct Just (const Nothing)

instance injectRight :: (Inject f g) => Inject f (Coproduct h g) where
  inj = Inr <<< inj
  prj = coproduct (const Nothing) prj

injC :: forall f g a. (Inject f g) => FreeC f a -> FreeC g a
injC fa = mapF (liftCoyonedaT inj) fa
