module Control.Monad.Free where

import Prelude

import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Data.Exists (Exists, mkExists, runExists)
import Unsafe.Coerce (unsafeCoerce)

foreign import data UnsafeBoundValue :: Type

foreign import data UnsafeBoundF :: Type -> Type

data Free f a
  = Pure a
  | Bind (f UnsafeBoundValue) (FreeBinds f UnsafeBoundValue a)

data FreeView f a b
  = PureView a
  | BindView (f b) (b -> Free f a)

data FreeBinds f a b
  = Leaf (a -> Free f b)
  | Node (FreeBinds f a UnsafeBoundValue) (FreeBinds f UnsafeBoundValue b)
  | Hoist (UnsafeBoundF ~> f) (FreeBinds UnsafeBoundF a b)

data FreeCons f a b
  = FreeCons (a -> Free f UnsafeBoundValue) (FreeBinds f UnsafeBoundValue b)

lift :: forall f a. f a -> Free f a
lift f = Bind (unsafeCoerce f) (unsafeCoerce (Leaf Pure))

roll :: forall f a. f (Free f a) -> Free f a
roll f = Bind (unsafeCoerce f) (unsafeCoerce (Leaf \a -> a))

suspend :: forall f a. Applicative f => Free f a -> Free f a
suspend = roll <<< pure

hoist :: forall f g. (f ~> g) -> Free f ~> Free g
hoist nat = case _ of
  Pure a -> Pure a
  Bind f k -> Bind (nat f) (Hoist (unsafeCoerce nat) (unsafeCoerce k))

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

resume ::
  forall f a r.
  (a -> r) ->
  (forall b. f b -> (b -> Free f a) -> r) ->
  Free f a ->
  r
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

run :: forall f m a. Functor f => Monad m => (f (Free f a) -> m (Free f a)) -> Free f a -> m a
run next = go where go = resume pure (\f k -> next (k <$> f) >>= go)

runRec :: forall f m a. Functor f => MonadRec m => (f (Free f a) -> m (Free f a)) -> Free f a -> m a
runRec next = tailRecM go <<< view
  where
  go = runExists case _ of
    PureView a -> pure $ Done a
    BindView f k -> Loop <<< view <$> next (k <$> f)

runPure :: forall f a. Functor f => (f (Free f a) -> Free f a) -> Free f a -> a
runPure next = go
  where
  go :: Free f a -> a
  go x = case unsafeCoerce (view x) :: FreeView f a UnsafeBoundValue of
    PureView a -> a
    BindView f k -> go (next (k <$> f))

interpret :: forall f m a. Monad m => (f ~> m) -> Free f a -> m a
interpret next = go where go = resume pure (\f k -> next f >>= k >>> go)

interpretRec :: forall f m a. MonadRec m => (f ~> m) -> Free f a -> m a
interpretRec nat = tailRecM go <<< view
  where
  go = runExists case _ of
    PureView a -> pure $ Done a
    BindView f k -> Loop <<< view <<< k <$> nat f
