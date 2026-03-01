module Control.Comonad.ZipCofree
  ( ZipCofree(..)
  ) where

import Control.Applicative (pure)
import Control.Apply (class Apply, lift2)
import Control.Comonad (class Comonad)
import Control.Comonad.Cofree (Cofree, deferCofree, head, tail)
import Control.Extend (class Extend)
import Control.Lazy as Z
import Data.Eq (class Eq, class Eq1)
import Data.Foldable (class Foldable)
import Data.FoldableWithIndex (class FoldableWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex)
import Data.Monoid (class Monoid)
import Data.Newtype (class Newtype)
import Data.Ord (class Ord, class Ord1)
import Data.Semigroup (class Semigroup)
import Data.Traversable (class Traversable)
import Data.TraversableWithIndex (class TraversableWithIndex)
import Data.Tuple (Tuple(..))
import Prelude (class Applicative, class Functor, ($))
import Safe.Coerce (coerce)

-- | `ZipCofree` is a newtype around `Cofree` which provides a zippy
-- | `Apply` instance.
newtype ZipCofree f a = ZipCofree (Cofree f a)

derive instance newtypeZipCofree :: Newtype (ZipCofree f a) _

derive newtype instance semigroupZipCofree :: (Apply f, Semigroup a) => Semigroup (ZipCofree f a)
derive newtype instance monoidZipCofree :: (Applicative f, Monoid a) => Monoid (ZipCofree f a)
derive newtype instance ordZipCofree :: (Ord1 f, Ord a) => Ord (ZipCofree f a)
derive newtype instance ord1ZipCofree :: (Ord1 f) => Ord1 (ZipCofree f)
derive newtype instance eqZipCofree :: (Eq1 f, Eq a) => Eq (ZipCofree f a)
derive newtype instance eq1ZipCofree :: (Eq1 f) => Eq1 (ZipCofree f)
derive newtype instance functorZipCofree :: Functor f => Functor (ZipCofree f)
derive newtype instance functorWithIndexZipCofree :: Functor f => FunctorWithIndex Int (ZipCofree f)
derive newtype instance extendZipCofree :: Functor f => Extend (ZipCofree f)
derive newtype instance comonadZipCofree :: Functor f => Comonad (ZipCofree f)
derive newtype instance foldableZipCofree :: Foldable f => Foldable (ZipCofree f)
derive newtype instance foldableWithIndexZipCofree :: Foldable f => FoldableWithIndex Int (ZipCofree f)
derive newtype instance traversableZipCofree :: Traversable f => Traversable (ZipCofree f)
derive newtype instance traversableWithIndexZipCofree :: Traversable f => TraversableWithIndex Int (ZipCofree f)
derive newtype instance lazyZipCofree :: Z.Lazy (ZipCofree f a)

instance applyZipCofree :: Apply f => Apply (ZipCofree f) where
  apply :: forall a b. ZipCofree f (a -> b) -> ZipCofree f a -> ZipCofree f b
  apply = coerce go
    where
    go :: Cofree f (a -> b) -> Cofree f a -> Cofree f b
    go ff aa = deferCofree \_ -> Tuple (head ff $ head aa) (lift2 go (tail ff) (tail aa))

instance applicativeZipCofree :: Applicative f => Applicative (ZipCofree f) where
  pure :: forall a. a -> ZipCofree f a
  pure = coerce go
    where
    go :: a -> Cofree f a
    go a = let node = deferCofree \_ -> Tuple a (pure node) in node
