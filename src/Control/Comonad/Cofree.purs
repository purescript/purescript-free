-- | The _cofree comonad_ for a `Functor`.

module Control.Comonad.Cofree
  ( module Control.Comonad.Cofree.Compat
  , explore
  , exploreM
  ) where

import Control.Comonad.Cofree.Compat hiding (explore, exploreM)

import Prelude (class Functor, map, (<$>))
import Control.Comonad (extract)
import Control.Monad.Free (Free, runRec)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State (State, StateT(..), runState, runStateT, state)
import Data.Tuple (Tuple(..))

-- | Explore a value in the cofree comonad by using an expression in a
-- | corresponding free monad.
-- |
-- | The free monad should be built from a functor which pairs with the
-- | functor underlying the cofree comonad.
explore
  :: forall f g a b
   . Functor f
  => Functor g
  => (forall x y. f (x -> y) -> g x -> y)
  -> Free f (a -> b)
  -> Cofree g a
  -> b
explore pair m w =
    case runState (runRec step m) w of
      Tuple f cof -> f (extract cof)
  where
    step :: f (Free f (a -> b)) -> State (Cofree g a) (Free f (a -> b))
    step ff = state \cof -> pair (map Tuple ff) (tail cof)

exploreM
  :: forall f g a b m
   . Functor f
  => Functor g
  => MonadRec m
  => (forall x y. f (x -> y) -> g x -> m y)
  -> Free f (a -> b)
  -> Cofree g a
  -> m b
exploreM pair m w =
  eval <$> runStateT (runRec step m) w
  where
    step :: f (Free f (a -> b)) -> StateT (Cofree g a) m (Free f (a -> b))
    step ff = StateT \cof -> pair (map Tuple ff) (tail cof)

    eval :: forall x y. Tuple (x -> y) (Cofree g x) -> y
    eval (Tuple f cof) = f (extract cof)
