-- | The _cofree comonad_ for a `Functor`.

module Control.Comonad.Cofree
  ( Cofree
  , mkCofree, (:<)
  , head
  , tail
  ) where

import Prelude

import Control.Comonad (class Comonad)
import Control.Alternative (class Alternative, (<|>), empty)
import Control.Extend (class Extend)
import Control.Monad.Trampoline (Trampoline, runTrampoline)

import Data.Foldable (class Foldable, foldr, foldl, foldMap)
import Data.Traversable (class Traversable, traverse)

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

infixr 5 mkCofree as :<

-- | Returns the label for a tree.
head :: forall f a. Cofree f a -> a
head (Cofree h _) = h

-- | Returns the "subtrees" of a tree.
tail :: forall f a. Cofree f a -> f (Cofree f a)
tail (Cofree _ t) = runTrampoline t

_tail :: forall f a. Cofree f a -> Trampoline (f (Cofree f a))
_tail (Cofree _ t) = t

_lift :: forall f a b. Functor f => (a -> b) -> Trampoline (f a) -> Trampoline (f b)
_lift = map <<< map

instance functorCofree :: Functor f => Functor (Cofree f) where
  map f = loop where
    loop fa = Cofree (f (head fa)) (_lift loop (_tail fa))

instance foldableCofree :: Foldable f => Foldable (Cofree f) where
  foldr f = flip go
    where
    go fa b = f (head fa) (foldr go b (tail fa))

  foldl f = go
    where
    go b fa = foldl go (f b (head fa)) (tail fa)

  foldMap f = go
    where
    go fa = f (head fa) <> (foldMap go (tail fa))

instance traversableCofree :: Traversable f => Traversable (Cofree f) where
  sequence = traverse id
  traverse f = loop
    where
    loop ta = mkCofree <$> f (head ta) <*> (traverse loop (tail ta))

instance extendCofree :: Functor f => Extend (Cofree f) where
  extend f = loop
    where
    loop fa = Cofree (f fa) (_lift loop (_tail fa))

instance comonadCofree :: Functor f => Comonad (Cofree f) where
  extract = head

instance applyCofree :: Apply f => Apply (Cofree f) where
  apply f x = mkCofree h t
    where
    h = (head f) (head x)
    t = apply <$> (tail f) <*> (tail x)

instance applicativeCofree :: Alternative f => Applicative (Cofree f) where
  pure a = mkCofree a empty

instance bindCofree :: Alternative f => Bind (Cofree f) where
  bind fa f = loop fa
    where
    loop fa =
      let fh = f (head fa)
      in mkCofree (head fh) ((tail fh) <|> (loop <$> tail fa))

instance monadCofree :: Alternative f => Monad (Cofree f)
