-- | The _cofree comonad_ for a `Functor`.

module Control.Comonad.Cofree
  ( Cofree()
  , mkCofree
  , head
  , tail
  ) where

import Prelude

import Control.Comonad (Comonad)
import Control.Alt ((<|>))
import Control.MonadPlus (MonadPlus)
import Control.Extend (Extend)
import Control.Monad.Trampoline (Trampoline(), runTrampoline)

import Data.Foldable (Foldable, foldr, foldl, foldMap)
import Data.Traversable (Traversable, traverse)

-- | The `Cofree` `Comonad` for a functor.
-- |
-- | A value of type `Cofree f a` consists of an `f`-branching
-- | tree, annotated with labels of type `a`.
-- |
-- | The `Comonad` instance supports _redecoration_, recomputing
-- | labels from the local context.
data Cofree f a = Cofree a (Trampoline (f (Cofree f a)))

-- | Create a value of type `Cofree f a` from a label and a
-- | functor-full of "subtrees".
mkCofree :: forall f a. a -> (f (Cofree f a)) -> Cofree f a
mkCofree a t = Cofree a (pure t)

-- | Returns the label for a tree.
head :: forall f a. Cofree f a -> a
head (Cofree h _) = h

-- | Returns the "subtrees" of a tree.
tail :: forall f a. Cofree f a -> f (Cofree f a)
tail (Cofree _ t) = runTrampoline t

_tail :: forall f a. Cofree f a -> Trampoline (f (Cofree f a))
_tail (Cofree _ t) = t

_lift :: forall f a b. (Functor f) => (a -> b) -> Trampoline (f a) -> Trampoline (f b)
_lift f = (<$>) $ (<$>) f

instance functorCofree :: (Functor f) => Functor (Cofree f) where
  map f = loop where
    loop fa = Cofree (f (head fa)) (_lift loop (_tail fa))

instance foldableCofree :: (Foldable f) => Foldable (Cofree f) where
  foldr f = flip go where
    go fa b = f a' b' where
      a' = head fa
      b' = foldr go b (tail fa)

  foldl f = go where
    go b fa = foldl go b' fa' where
      b'  = f b (head fa)
      fa' = tail fa

  foldMap f = go where
    go fa = f (head fa) ++ (foldMap go (tail fa))

instance traversableCofree :: (Traversable f) => Traversable (Cofree f) where
  traverse f = loop where
    loop ta = mkCofree <$> f (head ta) <*> (traverse loop (tail ta))

  sequence = traverse id

instance extendCofree :: (Functor f) => Extend (Cofree f) where
  extend f = loop where
    loop fa = Cofree (f fa) (_lift loop (_tail fa))

instance comonadCofree :: (Functor f) => Comonad (Cofree f) where
  extract = head

instance applyCofree :: (Apply f) => Apply (Cofree f) where
  apply f x = mkCofree h t where
    h = (head f) (head x)
    t = (<*>) <$> (tail f) <*> (tail x)

instance applicativeCofree :: (Applicative f) => Applicative (Cofree f) where
  pure a = mkCofree a (pure $ pure a)

instance bindCofree :: (MonadPlus f) => Bind (Cofree f) where
  bind fa f = loop fa where
    loop fa = let fh = f (head fa)
              in mkCofree (head fh) ((tail fh) <|> (loop <$> tail fa))

instance monadCofree :: (MonadPlus f) => Monad (Cofree f)
