module Control.Comonad.ZipCofree
  ( ZipCofree(..)
  ) where

import Prelude

import Control.Apply (lift2)
import Control.Comonad (class Comonad)
import Control.Comonad.Cofree (Cofree, deferCofree, head, tail)
import Control.Extend (class Extend)
import Control.Lazy as Z
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable)
import Data.FoldableWithIndex (class FoldableWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex)
import Data.Newtype (class Newtype)
import Data.Ord (class Ord1)
import Data.Traversable (class Traversable)
import Data.TraversableWithIndex (class TraversableWithIndex)
import Data.Tuple (Tuple(..))
import Safe.Coerce (coerce)

-- | `ZipCofree` is a newtype around `Cofree` which provides a zippy
-- | `Apply` instance.
newtype ZipCofree f a = ZipCofree (Cofree f a)

derive instance Newtype (ZipCofree f a) _

derive newtype instance (Apply f, Semigroup a) => Semigroup (ZipCofree f a)
derive newtype instance (Applicative f, Monoid a) => Monoid (ZipCofree f a)
derive newtype instance (Ord1 f, Ord a) => Ord (ZipCofree f a)
derive newtype instance Ord1 f => Ord1 (ZipCofree f)
derive newtype instance (Eq1 f, Eq a) => Eq (ZipCofree f a)
derive newtype instance Eq1 f => Eq1 (ZipCofree f)
derive newtype instance Functor f => Functor (ZipCofree f)
derive newtype instance Functor f => FunctorWithIndex Int (ZipCofree f)
derive newtype instance Functor f => Extend (ZipCofree f)
derive newtype instance Functor f => Comonad (ZipCofree f)
derive newtype instance Foldable f => Foldable (ZipCofree f)
derive newtype instance Foldable f => FoldableWithIndex Int (ZipCofree f)
derive newtype instance Traversable f => Traversable (ZipCofree f)
derive newtype instance Traversable f => TraversableWithIndex Int (ZipCofree f)
derive newtype instance Z.Lazy (ZipCofree f a)

instance Apply f => Apply (ZipCofree f) where
  apply :: forall a b. ZipCofree f (a -> b) -> ZipCofree f a -> ZipCofree f b
  apply = coerce go
    where
    go :: Cofree f (a -> b) -> Cofree f a -> Cofree f b
    go ff aa = deferCofree \_ -> Tuple (head ff $ head aa) (lift2 go (tail ff) (tail aa))

instance Applicative f => Applicative (ZipCofree f) where
  pure :: forall a. a -> ZipCofree f a
  pure = coerce go
    where
    go :: a -> Cofree f a
    go a = let node = deferCofree \_ -> Tuple a (pure node) in node
