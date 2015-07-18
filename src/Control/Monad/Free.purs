module Control.Monad.Free
  ( Free(..), GosubF()
  , FreeC(..)
  , MonadFree, wrap
  , liftF, liftFC
  , pureF, pureFC
  , mapF, injC
  , runFree
  , runFreeM
  , runFreeC
  , runFreeCM
  ) where

import Prelude

import Control.Monad.Cont.Class (MonadCont, callCC)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (MonadEff, liftEff)
import Control.Monad.Error.Class (MonadError, throwError, catchError)
import Control.Monad.Reader.Class (MonadReader, ask, local)
import Control.Monad.Rec.Class (MonadRec, tailRecM)
import Control.Monad.RWS.Class (MonadRWS)
import Control.Monad.State.Class (MonadState, state)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Writer.Class (MonadWriter, writer, listen, pass)

import Data.Coyoneda
import Data.Either (Either(..), either)
import Data.Exists (Exists(), mkExists, runExists)
import Data.Identity (Identity(..), runIdentity)
import Data.Inject (Inject, inj)
import Data.Monoid (Monoid)

newtype GosubF f a i = GosubF { a :: Unit -> Free f i, f :: i -> Free f a }

gosub :: forall f a i. (Unit -> Free f i) -> (i -> Free f a) -> Free f a
gosub a f = Gosub $ mkExists $ GosubF { a: a, f: f}

-- | The free `Monad` for a `Functor`.
-- |
-- | The implementation defers the evaluation of monadic binds so that it
-- | is safe to use monadic tail recursion, for example.
data Free f a = Pure a
              | Free (f (Free f a))
              | Gosub (Exists (GosubF f a))

-- | The free `Monad` for an arbitrary type constructor.
type FreeC f = Free (Coyoneda f)

-- | The `MonadFree` class provides the `wrap` function, which lifts
-- | actions described by a generating functor into a monad.
-- |
-- | The canonical instance of `MonadFree f` is `Free f`.
class MonadFree f m where
  wrap :: forall a. f (m a) -> m a

instance functorFree :: (Functor f) => Functor (Free f) where
  map f (Pure a) = Pure (f a)
  map f g = liftA1 f g

instance applyFree :: (Functor f) => Apply (Free f) where
  apply = ap

instance applicativeFree :: (Functor f) => Applicative (Free f) where
  pure = Pure

instance bindFree :: (Functor f) => Bind (Free f) where
  bind (Gosub g) k = runExists (\(GosubF v) -> gosub v.a (\x -> gosub (\unit -> v.f x) k)) g
  bind a         k = gosub (\unit -> a) k

instance monadFree :: (Functor f) => Monad (Free f)

instance monadTransFree :: MonadTrans Free where
  lift f = Free $ do
    a <- f
    return (Pure a)

instance monadFreeFree :: (Functor f) => MonadFree f (Free f) where
  wrap = Free

instance monadContFree :: (MonadRec m, MonadCont m) => MonadCont (Free m) where
  callCC f = lift (callCC (retract <<< f <<< map lift))

instance monadEffFree :: (Monad m, MonadEff e m) => MonadEff e (Free m) where
  liftEff = lift <<< liftEff

instance monadErrorFree :: (MonadRec m, MonadError e m) => MonadError e (Free m) where
  throwError = lift <<< throwError
  catchError as f = lift (catchError (retract as) (retract <<< f))

instance monadReaderFree :: (Monad m, MonadReader r m) => MonadReader r (Free m) where
  ask = lift ask
  local f = mapF (local f)

instance monadRWSFree :: (MonadRec m, Monoid w, MonadReader r m, MonadWriter w m, MonadState s m) => MonadRWS r w s (Free m)

instance monadStateFree :: (Monad m, MonadState s m) => MonadState s (Free m) where
  state = lift <<< state

instance monadWriterFree :: (MonadRec m, MonadWriter e m) => MonadWriter e (Free m) where
  writer = lift <<< writer
  listen = lift <<< listen <<< retract
  pass = lift <<< pass <<< retract

-- | Lift an action described by the generating functor `f` into the monad `m`
-- | (usually `Free f`).
liftF :: forall f m a. (Functor f, Monad m, MonadFree f m) => f a -> m a
liftF = wrap <<< (<$>) return

-- | An implementation of `pure` for the `Free` monad.
pureF :: forall f a. (Applicative f) => a -> Free f a
pureF = Free <<< pure <<< Pure

-- | Lift an action described by the generating type constructor `f` into the monad
-- | `FreeC f`.
liftFC :: forall f a. f a -> FreeC f a
liftFC = liftF <<< liftCoyoneda

-- | An implementation of `pure` for the `FreeC` monad.
pureFC :: forall f a. (Applicative f) => a -> FreeC f a
pureFC = liftFC <<< pure

-- | Use a natural transformation to change the generating functor of a `Free` monad.
mapF :: forall f g a. (Functor f, Functor g) => Natural f g -> Free f a -> Free g a
mapF t fa = either (\s -> Free <<< t $ mapF t <$> s) Pure (resume fa)

-- | Embed computations in one `Free` monad as computations in the `Free` monad for
-- | a coproduct type constructor.
-- |
-- | This construction allows us to write computations which are polymorphic in the
-- | particular `Free` monad we use, allowing us to extend the functionality of
-- | our monad later.
injC :: forall f g a. (Inject f g) => FreeC f a -> FreeC g a
injC = mapF (liftCoyonedaT inj)

resume :: forall f a. (Functor f) => Free f a -> Either (f (Free f a)) a
resume f = case f of
  Pure x -> Right x
  Free x -> Left x
  g -> case resumeGosub g of
    Left l -> Left l
    Right r -> resume r
  where
  resumeGosub :: Free f a -> Either (f (Free f a)) (Free f a)
  resumeGosub (Gosub g) =
    runExists (\(GosubF v) -> case v.a unit of
                                   Pure a -> Right (v.f a)
                                   Free t -> Left ((\h -> h >>= v.f) <$> t)
                                   Gosub h -> runExists (\(GosubF w) -> Right (w.a unit >>= (\z -> w.f z >>= v.f))) h) g

-- | `runFree` runs a computation of type `Free f a`, using a function which unwraps a single layer of
-- | the functor `f` at a time.
runFree :: forall f a. (Functor f) => (f (Free f a) -> Free f a) -> Free f a -> a
runFree fn = runIdentity <<< runFreeM (Identity <<< fn)

-- | `runFreeM` runs a compuation of type `Free f a` in any `Monad` which supports tail recursion.
-- | See the `MonadRec` type class for more details.
runFreeM :: forall f m a. (Functor f, MonadRec m) => (f (Free f a) -> m (Free f a)) -> Free f a -> m a
runFreeM fn = tailRecM \f ->
  case resume f of
    Left fs -> Left <$> fn fs
    Right a -> return (Right a)

-- | `runFreeC` is the equivalent of `runFree` for type constructors transformed with `Coyoneda`,
-- | hence we have no requirement that `f` be a `Functor`.
runFreeC :: forall f a. (forall a. f a -> a) -> FreeC f a -> a
runFreeC nat = runIdentity <<< runFreeCM (Identity <<< nat)

-- | `runFreeCM` is the equivalent of `runFreeM` for type constructors transformed with `Coyoneda`,
-- | hence we have no requirement that `f` be a `Functor`.
runFreeCM :: forall f m a. (MonadRec m) => Natural f m -> FreeC f a -> m a
runFreeCM nat = runFreeM (liftCoyonedaTF nat)

retract :: forall f a. (MonadRec f) => Free f a -> f a
retract = runFreeM id
