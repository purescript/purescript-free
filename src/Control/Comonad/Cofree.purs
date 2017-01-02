-- | The _cofree comonad_ for a `Functor`.

module Control.Comonad.Cofree
  ( Cofree
  , mkCofree, (:<)
  , head
  , tail
  , hoistCofree
  , unfoldCofree
  , explore
  ) where

import Prelude
import Control.Monad.Free (Free, runFreeM)
import Control.Alternative (class Alternative, (<|>), empty)
import Control.Comonad (class Comonad, extract)
import Control.Extend (class Extend)
import Control.Monad.State (State, runState, state)
import Data.Foldable (class Foldable, foldr, foldl, foldMap)
import Data.Lazy (Lazy, force, defer)
import Data.Traversable (class Traversable, traverse)
import Data.Tuple (Tuple(..))

-- | The `Cofree` `Comonad` for a functor.
-- |
-- | A value of type `Cofree f a` consists of an `f`-branching
-- | tree, annotated with labels of type `a`.
-- |
-- | The `Comonad` instance supports _redecoration_, recomputing
-- | labels from the local context.
data Cofree f a = Cofree a (Lazy (f (Cofree f a)))

-- | Create a value of type `Cofree f a` from a label and a
-- | functor-full of "subtrees".
mkCofree :: forall f a. a -> f (Cofree f a) -> Cofree f a
mkCofree a t = Cofree a (defer \_ -> t)

infixr 5 mkCofree as :<

-- | Returns the label for a tree.
head :: forall f a. Cofree f a -> a
head (Cofree h _) = h

-- | Returns the "subtrees" of a tree.
tail :: forall f a. Cofree f a -> f (Cofree f a)
tail (Cofree _ t) = force t

_tail :: forall f a. Cofree f a -> Lazy (f (Cofree f a))
_tail (Cofree _ t) = t

_lift :: forall f a b. Functor f => (a -> b) -> Lazy (f a) -> Lazy (f b)
_lift = map <<< map

hoistCofree :: forall f g. Functor f => (f ~> g) -> Cofree f ~> Cofree g
hoistCofree nat cf = head cf :< nat (hoistCofree nat <$> tail cf)

unfoldCofree
  :: forall f s a
   . Functor f
  => s
  -> (s -> a)
  -> (s -> f s)
  -> Cofree f a
unfoldCofree s e n =
  Cofree (e s) (defer \_ -> map (\s1 -> unfoldCofree s1 e n) (n s))

-- | Explore a value in the cofree comonad by using an expression in a
-- | corresponding free monad.
-- |
-- | The free monad should be built from a functor which pairs with the
-- | functor underlying the cofree comonad.
explore
  :: forall f g a b
   . (Functor f, Functor g)
  => (forall x y. f (x -> y) -> g x -> y)
  -> Free f (a -> b)
  -> Cofree g a
  -> b
explore pair m w =
    case runState (runFreeM step m) w of
      Tuple f cof -> f (extract cof)
  where
    step :: f (Free f (a -> b)) -> State (Cofree g a) (Free f (a -> b))
    step ff = state \cof -> pair (map Tuple ff) (tail cof)

instance eqCofree :: (Eq (f (Cofree f a)), Eq a) => Eq (Cofree f a) where
  eq x y = head x == head y && tail x == tail y

instance ordCofree :: (Ord (f (Cofree f a)), Ord a) => Ord (Cofree f a) where
  compare x y =
    case compare (head x) (head y) of
      EQ -> compare (tail x) (tail y)
      r -> r

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
    loop fa' =
      let fh = f (head fa')
      in mkCofree (head fh) ((tail fh) <|> (loop <$> tail fa'))

instance monadCofree :: Alternative f => Monad (Cofree f)
