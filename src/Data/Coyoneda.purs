module Data.Coyoneda
  ( Coyoneda(..)
  , Natural(..)
  , coyoneda
  , liftCoyoneda
  , lowerCoyoneda
  , liftCoyonedaT
  , liftCoyonedaTF
  ) where

import Control.Comonad
import Control.Extend
import Control.Monad.Trans

newtype Coyoneda f a = Coyoneda (forall i. { k :: i -> a , fi :: f i })

type Natural f g = forall a. f a -> g a

instance functorCoyoneda :: Functor (Coyoneda f) where
  (<$>) f (Coyoneda v) = Coyoneda v { k = f <<< v.k }

instance applyCoyoneda :: (Apply f) => Apply (Coyoneda f) where
  (<*>) f g = liftCoyoneda $ lowerCoyoneda f <*> lowerCoyoneda g

instance applicativeCoyoneda :: (Applicative f) => Applicative (Coyoneda f) where
  pure = liftCoyoneda <<< pure

instance bindCoyoneda :: (Bind f) => Bind (Coyoneda f) where
  (>>=) (Coyoneda v) k = liftCoyoneda $ v.fi >>= lowerCoyoneda <<< k <<< v.k

instance monadCoyoneda :: (Monad f) => Monad (Coyoneda f)

instance monadTransCoyoneda :: MonadTrans Coyoneda where
  lift = liftCoyoneda

instance extendCoyoneda :: (Extend w) => Extend (Coyoneda w) where
  (<<=) f (Coyoneda w) = liftCoyoneda $ f <<< coyoneda w.k <<= w.fi

instance comonadCoyoneda :: (Comonad w) => Comonad (Coyoneda w) where
  extract (Coyoneda w) = w.k $ extract w.fi

coyoneda :: forall f a b. (a -> b) -> f a -> Coyoneda f b
coyoneda k fi = k <$> liftCoyoneda fi

--liftCoyoneda :: forall f a. f a -> Coyoneda f a
--liftCoyoneda fa = Coyoneda { k: id , fi: fa }
foreign import liftCoyoneda "function liftCoyoneda(fa){return {k: function(a){return a;}, fi: fa};}"
  :: forall f a. f a -> Coyoneda f a

lowerCoyoneda :: forall f a. (Functor f) => Coyoneda f a -> f a
lowerCoyoneda (Coyoneda v) = v.k <$> v.fi

liftCoyonedaT :: forall f g. Natural f g -> Natural (Coyoneda f) (Coyoneda g)
liftCoyonedaT nat = \(Coyoneda v) -> Coyoneda v { fi = nat v.fi }

liftCoyonedaTF :: forall f g. (Functor g) => Natural f g -> Natural (Coyoneda f) g
liftCoyonedaTF nat = lowerCoyoneda <<< liftCoyonedaT nat
