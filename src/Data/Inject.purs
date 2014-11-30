module Data.Inject
  ( Inject
  , inj, prj
  , injC
  ) where

import Control.Monad.Free (FreeC(), mapF)
import Data.Coyoneda (liftCoyonedaT)
import Data.Either (Either(..))
import Data.Functor.Coproduct (Coproduct(..), coproduct)
import Data.Maybe (Maybe(..))

class Inject f g where
  inj :: forall a. f a -> g a
  prj :: forall a. g a -> Maybe (f a)

instance injectReflexive :: Inject f f where
  inj = id
  prj = Just

instance injectLeft :: Inject f (Coproduct f g) where
  inj = Coproduct <<< Left
  prj = coproduct Just (const Nothing)

instance injectRight :: (Inject f g) => Inject f (Coproduct h g) where
  inj = Coproduct <<< Right <<< inj
  prj = coproduct (const Nothing) prj

injC :: forall f g a. (Inject f g) => FreeC f a -> FreeC g a
injC = mapF (liftCoyonedaT inj)
