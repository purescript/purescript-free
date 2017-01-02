module Control.Monad.Free
  ( Free
  , suspendF
  , liftF
  , liftFI
  , hoistFree
  , injF
  , foldFree
  , substFree
  , runFree
  , runFreeM
  , resume
  ) where

import Prelude

import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Monad.Trans.Class (class MonadTrans)

import Data.CatList (CatList, empty, snoc, uncons)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.Inject (class Inject, inj)
import Data.Maybe (Maybe(..))
import Data.Traversable (class Traversable, traverse)
import Data.Tuple (Tuple(..))

import Unsafe.Coerce (unsafeCoerce)

-- | The free monad for a type constructor `f`.
-- |
-- | Implemented in the spirit of [Relection without Remorse](http://okmij.org/ftp/Haskell/zseq.pdf),
-- | the free monad is represented using a sequential data structure in
-- | order to overcome the quadratic complexity of left-associated binds
-- | and traversal through the free monad structure.
data Free f a = Free (FreeView f Val Val) (CatList (ExpF f))

newtype ExpF f = ExpF (Val -> Free f Val)

data FreeView f a b = Return a | Bind (f b) (b -> Free f a)

data Val

instance eqFree :: (Functor f, Eq (f (Free f a)), Eq a) => Eq (Free f a) where
  eq x y = resume x == resume y

instance ordFree :: (Functor f, Ord (f (Free f a)), Ord a) => Ord (Free f a) where
  compare x y = compare (resume x) (resume y)

instance freeFunctor :: Functor (Free f) where
  map k f = pure <<< k =<< f

instance freeBind :: Bind (Free f) where
  bind (Free v s) k = Free v (snoc s (ExpF (unsafeCoerceBind k)))
    where
    unsafeCoerceBind :: forall a b. (a -> Free f b) -> Val -> Free f Val
    unsafeCoerceBind = unsafeCoerce

instance freeApplicative :: Applicative (Free f) where
  pure = fromView <<< Return

instance freeApply :: Apply (Free f) where
  apply = ap

instance freeMonad :: Monad (Free f)

instance freeMonadTrans :: MonadTrans Free where
  lift = liftF

instance freeMonadRec :: MonadRec (Free f) where
  tailRecM k a = k a >>= case _ of
    Loop b -> tailRecM k b
    Done r -> pure r

instance foldableFree :: (Functor f, Foldable f) => Foldable (Free f) where
  foldMap f = go
    where
    go = resume >>> case _ of
      Left fa -> foldMap go fa
      Right a -> f a
  foldl f = go
    where
    go r = resume >>> case _  of
      Left fa -> foldl go r fa
      Right a -> f r a
  foldr f = go
    where
    go r = resume >>> case _ of
      Left fa -> foldr (flip go) r fa
      Right a -> f a r

instance traversableFree :: Traversable f => Traversable (Free f) where
  traverse f = go
    where
    go = resume >>> case _ of
      Left fa -> join <<< liftF <$> traverse go fa
      Right a -> pure <$> f a
  sequence tma = traverse id tma

-- | Lift an impure value described by the generating type constructor `f` into
-- | the free monad.
liftF :: forall f. f ~> Free f
liftF f = fromView (Bind (unsafeCoerceF f) (pure <<< unsafeCoerceVal))
  where
  unsafeCoerceF :: forall a. f a -> f Val
  unsafeCoerceF = unsafeCoerce

  unsafeCoerceVal :: forall a. Val -> a
  unsafeCoerceVal = unsafeCoerce

-- | Lift an action described by the generating type constructor `f` into
-- | `Free g` using `Inject` to go from `f` to `g`.
liftFI :: forall f g. Inject f g => f ~> Free g
liftFI fa = liftF (inj fa)

-- | Suspend a value given the applicative functor `f` into the free monad.
suspendF :: forall f. Applicative f => Free f ~> Free f
suspendF f = fromView (Bind (unsafeCoerceF (pure f)) unsafeCoerceVal)
  where
  unsafeCoerceF :: forall a. f (Free f a) -> f Val
  unsafeCoerceF = unsafeCoerce

  unsafeCoerceVal :: forall a. Val -> Free f a
  unsafeCoerceVal = unsafeCoerce

-- | Use a natural transformation to change the generating type constructor of a
-- | free monad.
hoistFree :: forall f g. (f ~> g) -> Free f ~> Free g
hoistFree k = substFree (liftF <<< k)

-- | Embed computations in one `Free` monad as computations in the `Free` monad
-- | for a coproduct type constructor.
-- |
-- | This construction allows us to write computations which are polymorphic in
-- | the particular `Free` monad we use, allowing us to extend the functionality
-- | of our monad later.
injF :: forall f g. Inject f g => Free f ~> Free g
injF = hoistFree inj

-- | Run a free monad with a natural transformation from the type constructor `f`
-- | to the tail-recursive monad `m`. See the `MonadRec` type class for more
-- | details.
foldFree :: forall f m. MonadRec m => (f ~> m) -> Free f ~> m
foldFree k = tailRecM go
  where
  go :: forall a. Free f a -> m (Step (Free f a) a)
  go f = case toView f of
    Return a -> Done <$> pure a
    Bind g i -> (Loop <<< i) <$> k g

-- | Like `foldFree`, but for folding into some other Free monad without the
-- | overhead that `MonadRec` incurs.
substFree :: forall f g. (f ~> Free g) -> Free f ~> Free g
substFree k = go
  where
  go :: Free f ~> Free g
  go f = case toView f of
    Return a -> pure a
    Bind g i -> k g >>= go <$> i

-- | Run a free monad with a function that unwraps a single layer of the functor
-- | `f` at a time.
runFree :: forall f a. Functor f => (f (Free f a) -> Free f a) -> Free f a -> a
runFree k = go
  where
  go :: Free f a -> a
  go f = case toView f of
    Return a -> a
    Bind g i -> go (k (i <$> g))

-- | Run a free monad with a function mapping a functor `f` to a tail-recursive
-- | monad `m`. See the `MonadRec` type class for more details.
runFreeM
  :: forall f m a
   . (Functor f, MonadRec m)
  => (f (Free f a) -> m (Free f a))
  -> Free f a
  -> m a
runFreeM k = tailRecM go
  where
  go :: Free f a -> m (Step (Free f a) a)
  go f = case toView f of
    Return a -> Done <$> pure a
    Bind g i -> Loop <$> k (i <$> g)

-- | Unwraps a single layer of the functor `f`.
resume
  :: forall f a
   . Functor f
  => Free f a
  -> Either (f (Free f a)) a
resume f = case toView f of
  Return a -> Right a
  Bind g i -> Left (i <$> g)

fromView :: forall f a. FreeView f a Val -> Free f a
fromView f = Free (unsafeCoerceFreeView f) empty
  where
  unsafeCoerceFreeView :: FreeView f a Val -> FreeView f Val Val
  unsafeCoerceFreeView = unsafeCoerce

toView :: forall f a. Free f a -> FreeView f a Val
toView (Free v s) =
  case v of
    Return a ->
      case uncons s of
        Nothing ->
          Return (unsafeCoerceVal a)
        Just (Tuple h t) ->
          toView (unsafeCoerceFree (concatF ((runExpF h) a) t))
    Bind f k ->
      Bind f (\a -> unsafeCoerceFree (concatF (k a) s))
  where
  concatF :: Free f Val -> CatList (ExpF f) -> Free f Val
  concatF (Free v' l) r = Free v' (l <> r)

  runExpF :: ExpF f -> (Val -> Free f Val)
  runExpF (ExpF k) = k

  unsafeCoerceFree :: Free f Val -> Free f a
  unsafeCoerceFree = unsafeCoerce

  unsafeCoerceVal :: Val -> a
  unsafeCoerceVal = unsafeCoerce
