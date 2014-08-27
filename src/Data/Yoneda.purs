module Data.Yoneda
  ( Yoneda(..)
  , runYoneda
  , liftYoneda
  , lowerYoneda
  ) where

import Control.Comonad
import Control.Extend
import Control.Monad.Trans

newtype Yoneda f a = Yoneda (forall b. (a -> b) -> f b)

instance functorYoneda :: Functor (Yoneda f) where
  (<$>) f m = Yoneda (\k -> runYoneda m (k <<< f))

instance applyYoneda :: (Apply f) => Apply (Yoneda f) where
  (<*>) (Yoneda f) (Yoneda g) = Yoneda (\k -> (f $ (<<<) k) <*> g id)

instance applicativeYoneda :: (Applicative f) => Applicative (Yoneda f) where
  pure = liftYoneda <<< pure

instance bindCoyoneda :: (Bind f) => Bind (Yoneda f) where
  (>>=) (Yoneda f) g = Yoneda (\k -> f id >>= \a -> runYoneda (g a) k)

instance monadYoneda :: (Monad f) => Monad (Yoneda f)

instance monadTransYoneda :: MonadTrans Yoneda where
  lift = liftYoneda

instance extendYoneda :: (Extend w) => Extend (Yoneda w) where
  (<<=) f (Yoneda w) = Yoneda (\k -> k <<< f <<< liftYoneda <<= w id)

instance comonadYoneda :: (Comonad w) => Comonad (Yoneda w) where
  extract = extract <<< lowerYoneda

runYoneda :: forall f a b. Yoneda f a -> (a -> b) -> f b
runYoneda (Yoneda f) k = f k

liftYoneda :: forall f a. (Functor f) => f a -> Yoneda f a
liftYoneda m = Yoneda (\k -> k <$> m)

lowerYoneda :: forall f a. Yoneda f a -> f a
lowerYoneda (Yoneda k) = k id
