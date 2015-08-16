-- | A _trampoline_ monad, which can be used at the bottom of
-- | a monad transformer stack to avoid stack overflows in large
-- | monadic computations.

module Benchmark.Trampoline0df59c5
  ( Trampoline()
  , done
  , suspend
  , delay'
  , delay
  , runTrampoline
  ) where

import Prelude

import Benchmark.Free0df59c5

import Data.Lazy
import Data.Foldable 
import Data.Traversable

-- | The `Trampoline` monad
-- |
-- | A computation of type `Trampoline a` consists of zero or more lazy 
-- | suspensions before a value is returned.
type Trampoline = Free Lazy

-- | Return a value immediately
done :: forall a. a -> Trampoline a
done = pure

-- | Suspend a computation by one step.
suspend :: forall a. Trampoline a -> Trampoline a
suspend t = Free (defer (const t))

-- | Use the `Trampoline` monad to represent a `Lazy` value.
delay' :: forall a. Lazy a -> Trampoline a
delay' a = Free (done <$> a)

-- | Use the `Trampoline` monad to represent the delayed evaluation of a value.
delay :: forall a. (Unit -> a) -> Trampoline a
delay = delay' <<< defer

-- | Run a computation in the `Trampoline` monad.
runTrampoline :: forall a. Trampoline a -> a
runTrampoline = runFree force
