## Module Control.Monad.Free

#### `Free`

``` purescript
data Free f a
```

The free monad for a type constructor `f`.

Implemented in the spirit of [Relection without Remorse](http://okmij.org/ftp/Haskell/zseq.pdf),
the free monad is represented using a sequential data structure in
order to overcome the quadratic complexity of left-associated binds
and traversal through the free monad structure.

##### Instances
``` purescript
instance freeFunctor :: Functor (Free f)
instance freeBind :: Bind (Free f)
instance freeApplicative :: Applicative (Free f)
instance freeApply :: Apply (Free f)
instance freeMonad :: Monad (Free f)
instance freeMonadTrans :: MonadTrans Free
instance freeMonadRec :: MonadRec (Free f)
```

#### `liftF`

``` purescript
liftF :: forall f a. f a -> Free f a
```

Lift an impure value described by the generating type constructor `f` into the free monad.

#### `liftFI`

``` purescript
liftFI :: forall f g a. (Inject f g) => f a -> Free g a
```

Lift an action described by the generating type constructor `f` into
`Free g` using `Inject` to go from `f` to `g`.

#### `suspendF`

``` purescript
suspendF :: forall f a. (Applicative f) => Free f a -> Free f a
```

Suspend a value given the applicative functor `f` into the free monad.

#### `mapF`

``` purescript
mapF :: forall f g a. NaturalTransformation f g -> Free f a -> Free g a
```

Use a natural transformation to change the generating type constructor of a free monad.

#### `injF`

``` purescript
injF :: forall f g a. (Inject f g) => Free f a -> Free g a
```

Embed computations in one `Free` monad as computations in the `Free` monad for
a coproduct type constructor.

This construction allows us to write computations which are polymorphic in the
particular `Free` monad we use, allowing us to extend the functionality of
our monad later.

#### `foldMapF`

``` purescript
foldMapF :: forall f m a. (MonadRec m) => NaturalTransformation f m -> Free f a -> m a
```

Run a free monad with a natural transformation from the type constructor `f`
to the tail-recursive monad `m`.See the `MonadRec` type class for more details.

#### `runFree`

``` purescript
runFree :: forall f a. (Functor f) => (f (Free f a) -> Free f a) -> Free f a -> a
```

Run a free monad with a function that unwraps a single layer of the functor `f` at a time,

#### `runFreeM`

``` purescript
runFreeM :: forall f m a. (Functor f, MonadRec m) => (f (Free f a) -> m (Free f a)) -> Free f a -> m a
```

Run a free monad with a function mapping a functor `f` to a tail-recursive monad `m`.
See the `MonadRec` type class for more details.


