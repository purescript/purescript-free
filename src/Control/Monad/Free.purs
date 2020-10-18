module Control.Monad.Free
  ( Free
  , lift
  , roll
  , suspend
  , hoist
  , resume
  , run
  , runRec
  , runPure
  , interpret
  , interpretRec
  )
  where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Monad.Trans.Class (class MonadTrans)
import Data.Either (Either(..))
import Data.Eq (class Eq1, eq1)
import Data.Exists (Exists, mkExists, runExists)
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.Ord (class Ord1, compare1)
import Data.Traversable (class Traversable, traverse)
import Unsafe.Coerce (unsafeCoerce)

foreign import data UnsafeBoundValue :: Type

foreign import data UnsafeBoundF :: Type -> Type

data Free f a
  = Pure a
  | Bind (f UnsafeBoundValue) (FreeBinds f UnsafeBoundValue a)

instance eqFree :: (Functor f, Eq1 f, Eq a) => Eq (Free f a) where
  eq x y = case resume' x, resume' y of
    Left fa, Left fb -> eq1 fa fb
    Right a, Right b -> eq a b
    _, _ -> false

instance ordFree :: (Functor f, Ord1 f, Ord a) => Ord (Free f a) where
  compare x y = case resume' x, resume' y of
    Left fa, Left fb -> compare1 fa fb
    Left _, _ -> LT
    _, Left _ -> GT
    Right a, Right b -> compare a b

instance eq1Free :: (Functor f, Eq1 f) => Eq1 (Free f) where
  eq1 = eq

instance ord1Free :: (Functor f, Ord1 f, Ord a) => Ord1 (Free f) where
  compare1 = compare

instance functorFree :: Functor (Free f) where
  map f (Pure a) = Pure (f a)
  map f (Bind a bs) = Bind a (Node (unsafeCoerce bs) (Leaf (Pure <<< unsafeCoerce f)))

instance applyFree :: Apply (Free f) where
  apply = ap

instance applicativeFree :: Applicative (Free f) where
  pure = Pure

instance bindFree :: Bind (Free f) where
  bind (Pure a) k = k a
  bind (Bind a bs) k = Bind a (Node (unsafeCoerce bs) (Leaf (unsafeCoerce k)))

instance monadFree :: Monad (Free f)

instance monadTransFree :: MonadTrans Free where
  lift = lift

instance monadRecFree :: MonadRec (Free f) where
  tailRecM k a = k a >>= case _ of
    Loop b -> tailRecM k b
    Done r -> pure r

instance foldableFree :: (Functor f, Foldable f) => Foldable (Free f) where
  foldMap f = go
    where
    go = resume' >>> case _ of
      Left fa -> foldMap go fa
      Right a -> f a

  foldl f = go
    where
    go r = resume' >>> case _ of
      Left fa -> foldl go r fa
      Right a -> f r a

  foldr f = go
    where
    go r = resume' >>> case _ of
      Left fa -> foldr (flip go) r fa
      Right a -> f a r

instance traversableFree :: Traversable f => Traversable (Free f) where
  traverse f = go
    where
    go = resume' >>> case _ of
      Left fa -> join <<< lift <$> traverse go fa
      Right a -> pure <$> f a

  sequence tma = traverse identity tma

instance semigroupFree :: Semigroup a => Semigroup (Free f a) where
  append = lift2 append

instance monoidFree :: Monoid a => Monoid (Free f a) where
  mempty = pure mempty

data FreeView f a b
  = PureView a
  | BindView (f b) (b -> Free f a)

data FreeBinds f a b
  = Leaf (a -> Free f b)
  | Node (FreeBinds f a UnsafeBoundValue) (FreeBinds f UnsafeBoundValue b)
  | Hoist (UnsafeBoundF ~> f) (FreeBinds UnsafeBoundF a b)

data FreeCons f a b
  = FreeCons (a -> Free f UnsafeBoundValue) (FreeBinds f UnsafeBoundValue b)

-- | Lift an impure value described by the generating type constructor `f` into
-- | the free monad.
lift :: forall f a. f a -> Free f a
lift f = Bind (unsafeCoerce f) (unsafeCoerce (Leaf Pure))

-- | Add a layer.
roll :: forall f a. f (Free f a) -> Free f a
roll f = Bind (unsafeCoerce f) (unsafeCoerce (Leaf \a -> a))

 -- | Suspend a value given the applicative functor `f` into the free monad.
suspend :: forall f a. Applicative f => Free f a -> Free f a
suspend f = roll (pure f)

-- | Use a natural transformation to change the generating type constructor of a
-- | free monad.
hoist :: forall f g. (f ~> g) -> Free f ~> Free g
hoist nat = case _ of
  Pure a -> Pure a
  Bind f k -> Bind (nat f) (Hoist (unsafeCoerce nat) (unsafeCoerce k))

 -- | Unwraps a single layer of `f`, providing the continuation.
resume
  :: forall f a r
   . (a -> r)
  -> (forall b. f b -> (b -> Free f a) -> r)
  -> Free f a
  -> r
resume pure' bind' = case _ of
  Pure a -> pure' a
  Bind a bs -> bind' a (go1 bs)
  where
  go1 :: forall x y. FreeBinds f x y -> x -> Free f y
  go1 bs x = case bs of
    Leaf k -> k x
    Node l r -> case uncons l r of
      FreeCons k bs' -> case k x of
        Pure a -> go1 bs' a
        Bind a bs'' -> Bind a (Node bs'' bs')
    Hoist nat bs' ->
      go2 nat bs' x

  go2 :: forall g x y. (UnsafeBoundF ~> g) -> FreeBinds UnsafeBoundF x y -> x -> Free g y
  go2 nat bs x = case bs of
    Leaf k -> hoist nat (k x)
    Node l r -> case uncons l r of
      FreeCons k bs' -> case k x of
        Pure a -> go2 nat bs' a
        Bind a bs'' -> Bind (nat a) (Hoist nat (Node bs'' bs'))
    Hoist nat' bs' ->
      go2 (nat <<< nat') bs' x

run :: forall f m a. Functor f => Monad m => (f (Free f a) -> m (Free f a)) -> Free f a -> m a
run next = go where go = resume pure (\f k -> next (k <$> f) >>= go)

-- | Run a free monad with a function mapping a functor `f` to a tail-recursive
-- | monad `m`. See the `MonadRec` type class for more details.
runRec :: forall f m a. Functor f => MonadRec m => (f (Free f a) -> m (Free f a)) -> Free f a -> m a
runRec next = tailRecM go <<< view
  where
  go = runExists case _ of
    PureView a -> pure $ Done a
    BindView f k -> Loop <<< view <$> next (k <$> f)

-- | Run a free monad with a function that unwraps a single layer of the functor
-- | `f` at a time.
runPure :: forall f a. Functor f => (f (Free f a) -> Free f a) -> Free f a -> a
runPure next = go
  where
  go :: Free f a -> a
  go x = case unsafeCoerce (view x) :: FreeView f a UnsafeBoundValue of
    PureView a -> a
    BindView f k -> go (next (k <$> f))

-- | Run a free monad with a natural transformation from the type constructor `f`
-- | to the monad `m`, which can be some other Free monad. If you need tail
-- | recursion for stack safety, see `interpretRec`.
interpret :: forall f m a. Monad m => (f ~> m) -> Free f a -> m a
interpret next = go where go = resume pure (\f k -> next f >>= k >>> go)

-- | Run a free monad with a natural transformation from the type constructor `f`
-- | to the tail-recursive monad `m`. See the `MonadRec` type class for more
-- | details.
interpretRec :: forall f m a. MonadRec m => (f ~> m) -> Free f a -> m a
interpretRec nat = tailRecM go <<< view
  where
  go = runExists case _ of
    PureView a -> pure $ Done a
    BindView f k -> Loop <<< view <<< k <$> nat f

resume' :: forall f a. Functor f => Free f a -> Either (f (Free f a)) a
resume' = resume Right (\g i -> Left (map i g))

uncons :: forall f a b x. FreeBinds f a x -> FreeBinds f x b -> FreeCons f a b
uncons = go1
  where
  go1 :: forall a' b' x'. FreeBinds f a' x' -> FreeBinds f x' b' -> FreeCons f a' b'
  go1 l r = case l of
    Leaf k -> FreeCons (unsafeCoerce k) (unsafeCoerce r)
    Node l' r' -> go1 l' (Node (unsafeCoerce r') (unsafeCoerce r))
    Hoist nat l' -> go2 nat l' r

  go2 :: forall g a' b' x'. (UnsafeBoundF ~> g) -> FreeBinds UnsafeBoundF a' x' -> FreeBinds g x' b' -> FreeCons g a' b'
  go2 nat l r = case l of
    Leaf k -> FreeCons (hoist nat <$> unsafeCoerce k) (unsafeCoerce r)
    Node l' r' -> go2 nat l' (Node (Hoist nat (unsafeCoerce r')) (unsafeCoerce r))
    Hoist nat' n -> go2 (nat <<< nat') n r

view :: forall f a. Free f a -> Exists (FreeView f a)
view = resume (mkExists <<< PureView) \a b -> mkExists (BindView a b)
