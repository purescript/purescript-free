module Control.Monad.Free.Compat
  ( module Control.Monad.Free
  , suspendF
  , wrap
  , liftF
  , hoistFree
  , foldFree
  , substFree
  , runFree
  , runFreeM
  , resume
  , resume'
  ) where

import Prelude

import Control.Monad.Free (Free)
import Control.Monad.Free as Free
import Control.Monad.Rec.Class (class MonadRec)
import Data.Either (Either(..))

-- | Lift an impure value described by the generating type constructor `f` into
-- | the free monad.
liftF :: forall f. f ~> Free f
liftF = Free.lift

-- | Add a layer.
wrap :: forall f a. f (Free f a) -> Free f a
wrap = Free.roll

-- | Suspend a value given the applicative functor `f` into the free monad.
suspendF :: forall f. Applicative f => Free f ~> Free f
suspendF = Free.suspend

-- | Use a natural transformation to change the generating type constructor of a
-- | free monad.
hoistFree :: forall f g. (f ~> g) -> Free f ~> Free g
hoistFree = Free.hoist

-- | Run a free monad with a natural transformation from the type constructor `f`
-- | to the tail-recursive monad `m`. See the `MonadRec` type class for more
-- | details.
foldFree :: forall f m. MonadRec m => (f ~> m) -> Free f ~> m
foldFree = Free.interpretRec

-- | Like `foldFree`, but for folding into some other Free monad without the
-- | overhead that `MonadRec` incurs.
substFree :: forall f g. (f ~> Free g) -> Free f ~> Free g
substFree = Free.interpret

-- | Run a free monad with a function that unwraps a single layer of the functor
-- | `f` at a time.
runFree :: forall f a. Functor f => (f (Free f a) -> Free f a) -> Free f a -> a
runFree = Free.run

-- | Run a free monad with a function mapping a functor `f` to a tail-recursive
-- | monad `m`. See the `MonadRec` type class for more details.
runFreeM
  :: forall f m a
   . Functor f
  => MonadRec m
  => (f (Free f a) -> m (Free f a))
  -> Free f a
  -> m a
runFreeM = Free.runRec

-- | Unwraps a single layer of the functor `f`.
resume
  :: forall f a
   . Functor f
  => Free f a
  -> Either (f (Free f a)) a
resume = resume' (\g i -> Left (i <$> g)) Right

-- | Unwraps a single layer of `f`, providing the continuation.
resume'
  :: forall f a r
   . (forall b. f b -> (b -> Free f a) -> r)
  -> (a -> r)
  -> Free f a
  -> r
resume' k j f = Free.resume j k f
