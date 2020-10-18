module Benchmark.Free0df59c5
  ( Free(..)
  , GosubF
  , FreeC(..)
  , class MonadFree
  , wrap
  , Natural
  , liftF
  , liftFI
  , liftFC
  , liftFCI
  , pureF
  , pureFC
  , mapF
  , mapFC
  , bindF
  , bindFC
  , injF
  , injFC
  , runFree
  , runFreeM
  , runFreeC
  , runFreeCM
  ) where

import Prelude

import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Monad.Trans.Class (class MonadTrans)
import Data.Coyoneda (Coyoneda, hoistCoyoneda, liftCoyoneda, lowerCoyoneda)
import Data.Either (Either(..), either)
import Data.Exists (Exists, mkExists, runExists)
import Data.Functor.Coproduct.Inject (class Inject, inj)
import Data.Identity (Identity(..))
import Data.Newtype (unwrap)
import Partial.Unsafe (unsafePartialBecause)

type Natural f g = forall a. f a -> g a

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

instance functorFree :: Functor f => Functor (Free f) where
  map f (Pure a) = Pure (f a)
  map f g = liftA1 f g

instance applyFree :: Functor f => Apply (Free f) where
  apply = ap

instance applicativeFree :: Functor f => Applicative (Free f) where
  pure = Pure

instance bindFree :: Functor f => Bind (Free f) where
  bind (Gosub g) k = runExists (\(GosubF v) -> gosub v.a (\x -> gosub (\unit -> v.f x) k)) g
  bind a         k = gosub (\unit -> a) k

instance monadFree :: Functor f => Monad (Free f)

instance monadTransFree :: MonadTrans Free where
  lift f = Free $ do
    a <- f
    pure (Pure a)

instance monadFreeFree :: Functor f => MonadFree f (Free f) where
  wrap = Free

instance monadRecFree :: Functor f => MonadRec (Free f) where
  tailRecM f u = f u >>= \o -> case o of
                                    Loop a -> tailRecM f a
                                    Done b -> pure b

-- | Lift an action described by the generating functor `f` into the monad `m`
-- | (usually `Free f`).
liftF :: forall f m a. Functor f => Monad m => MonadFree f m  => f a -> m a
liftF = wrap <<< map pure

-- | Lift an action described by the generating type constructor `f` into
-- | `Free g` using `Inject` to go from `f` to `g`.
liftFI :: forall f g a. Inject f g => Functor g => f a -> Free g a
liftFI fa = liftF (inj fa :: g a)

-- | Lift an action described by the generating type constructor `f` into the monad
-- | `FreeC f`.
liftFC :: forall f a. f a -> FreeC f a
liftFC = liftF <<< liftCoyoneda

-- | Lift an action described by the generating type constructor `f` into
-- | `FreeC g` using `Inject` to go from `f` to `g`.
liftFCI :: forall f g a. Inject f g => f a -> FreeC g a
liftFCI fa = liftFC (inj fa :: g a)

-- | An implementation of `pure` for the `Free` monad.
pureF :: forall f a. Applicative f => a -> Free f a
pureF = Free <<< pure <<< Pure

-- | An implementation of `pure` for the `FreeC` monad.
pureFC :: forall f a. Applicative f => a -> FreeC f a
pureFC = liftFC <<< pure

-- | Use a natural transformation to change the generating functor of a `Free` monad.
mapF :: forall f g a. Functor f => Functor g => Natural f g -> Free f a -> Free g a
mapF t fa = either (\s -> Free <<< t $ mapF t <$> s) Pure (resume fa)

-- | Use a natural transformation to change the generating type constructor of
-- | a `FreeC` monad to another functor.
mapFC :: forall f g a. Functor g => Natural f g -> FreeC f a -> Free g a
mapFC t = mapF (lowerCoyoneda <<< hoistCoyoneda t)

-- | Use a natural transformation to interpret one `Free` monad as another.
bindF :: forall f g a. Functor f => Functor g => Free f a -> Natural f (Free g) -> Free g a
bindF fa t = either (\m -> t m >>= \fa' -> bindF fa' t) Pure (resume fa)

-- | Use a natural transformation to interpret a `FreeC` monad as a different
-- | `Free` monad.
bindFC :: forall f g a. Functor g => FreeC f a -> Natural f (Free g) -> Free g a
bindFC fa t = bindF fa (lowerCoyoneda <<< hoistCoyoneda t)

-- | Embed computations in one `Free` monad as computations in the `Free` monad for
-- | a coproduct type constructor.
-- |
-- | This construction allows us to write computations which are polymorphic in the
-- | particular `Free` monad we use, allowing us to extend the functionality of
-- | our monad later.
injF :: forall f g a. Functor f => Functor g => Inject f g => Free f a -> Free g a
injF = mapF inj

-- | Embed computations in one `FreeC` monad as computations in the `FreeC` monad for
-- | a coproduct type constructor.
-- |
-- | This construction allows us to write computations which are polymorphic in the
-- | particular `Free` monad we use, allowing us to extend the functionality of
-- | our monad later.
injFC :: forall f g a. Inject f g => FreeC f a -> FreeC g a
injFC = mapF (hoistCoyoneda inj)

resume :: forall f a. Functor f => Free f a -> Either (f (Free f a)) a
resume f = case f of
  Pure x -> Right x
  Free x -> Left x
  g -> unsafePartialBecause "Existing implementation." case resumeGosub g of
    Left l -> Left l
    Right r -> resume r
  where
  resumeGosub :: Partial => Free f a -> Either (f (Free f a)) (Free f a)
  resumeGosub (Gosub g) =
    runExists (\(GosubF v) -> case v.a unit of
                                   Pure a -> Right (v.f a)
                                   Free t -> Left ((\h -> h >>= v.f) <$> t)
                                   Gosub h -> runExists (\(GosubF w) -> Right (w.a unit >>= (\z -> w.f z >>= v.f))) h) g

-- | `runFree` runs a computation of type `Free f a`, using a function which unwraps a single layer of
-- | the functor `f` at a time.
runFree :: forall f a. Functor f => (f (Free f a) -> Free f a) -> Free f a -> a
runFree fn = unwrap <<< runFreeM (Identity <<< fn)

-- | `runFreeM` runs a compuation of type `Free f a` in any `Monad` which supports tail recursion.
-- | See the `MonadRec` type class for more details.
runFreeM :: forall f m a. Functor f => MonadRec m => (f (Free f a) -> m (Free f a)) -> Free f a -> m a
runFreeM fn = tailRecM \f ->
  case resume f of
    Left fs -> Loop <$> fn fs
    Right a -> pure (Done a)

-- | `runFreeC` is the equivalent of `runFree` for type constructors transformed with `Coyoneda`,
-- | hence we have no requirement that `f` be a `Functor`.
runFreeC :: forall f a. (forall b. f b -> b) -> FreeC f a -> a
runFreeC nat = unwrap <<< runFreeCM (Identity <<< nat)

-- | `runFreeCM` is the equivalent of `runFreeM` for type constructors transformed with `Coyoneda`,
-- | hence we have no requirement that `f` be a `Functor`.
runFreeCM :: forall f m a. (MonadRec m) => Natural f m -> FreeC f a -> m a
runFreeCM nat = runFreeM (lowerCoyoneda <<< hoistCoyoneda nat)
