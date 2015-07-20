## Module Control.Monad.Free

#### `GosubF`

``` purescript
newtype GosubF f a i
```

#### `Free`

``` purescript
data Free f a
  = Pure a
  | Free (f (Free f a))
  | Gosub (Exists (GosubF f a))
```

The free `Monad` for a `Functor`.

The implementation defers the evaluation of monadic binds so that it
is safe to use monadic tail recursion, for example.

##### Instances
``` purescript
instance functorFree :: (Functor f) => Functor (Free f)
instance applyFree :: (Functor f) => Apply (Free f)
instance applicativeFree :: (Functor f) => Applicative (Free f)
instance bindFree :: (Functor f) => Bind (Free f)
instance monadFree :: (Functor f) => Monad (Free f)
instance monadTransFree :: MonadTrans Free
instance monadFreeFree :: (Functor f) => MonadFree f (Free f)
```

#### `FreeC`

``` purescript
type FreeC f = Free (Coyoneda f)
```

The free `Monad` for an arbitrary type constructor.

#### `MonadFree`

``` purescript
class MonadFree f m where
  wrap :: forall a. f (m a) -> m a
```

The `MonadFree` class provides the `wrap` function, which lifts
actions described by a generating functor into a monad.

The canonical instance of `MonadFree f` is `Free f`.

##### Instances
``` purescript
instance monadFreeFree :: (Functor f) => MonadFree f (Free f)
```

#### `liftF`

``` purescript
liftF :: forall f m a. (Functor f, Monad m, MonadFree f m) => f a -> m a
```

Lift an action described by the generating functor `f` into the monad `m`
(usually `Free f`).

#### `liftFI`

``` purescript
liftFI :: forall f g a. (Inject f g, Functor g) => f a -> Free g a
```

Lift an action described by the generating type constructor `f` into
`Free g` using `Inject` to go from `f` to `g`.

#### `pureF`

``` purescript
pureF :: forall f a. (Applicative f) => a -> Free f a
```

An implementation of `pure` for the `Free` monad.

#### `liftFC`

``` purescript
liftFC :: forall f a. f a -> FreeC f a
```

Lift an action described by the generating type constructor `f` into the monad
`FreeC f`.

#### `liftFCI`

``` purescript
liftFCI :: forall f g a. (Inject f g) => f a -> FreeC g a
```

Lift an action described by the generating type constructor `f` into
`FreeC g` using `Inject` to go from `f` to `g`.

#### `pureFC`

``` purescript
pureFC :: forall f a. (Applicative f) => a -> FreeC f a
```

An implementation of `pure` for the `FreeC` monad.

#### `mapF`

``` purescript
mapF :: forall f g a. (Functor f, Functor g) => Natural f g -> Free f a -> Free g a
```

Use a natural transformation to change the generating functor of a `Free` monad.

#### `mapFC`

``` purescript
mapFC :: forall f g a. (Functor g) => Natural f g -> FreeC f a -> Free g a
```

Use a natural transformation to change the generating type constructor of
a `FreeC` monad to another functor.

#### `bindF`

``` purescript
bindF :: forall f g a. (Functor f, Functor g) => Free f a -> Natural f (Free g) -> Free g a
```

Use a natural transformation to interpret one `Free` monad as another.

#### `bindFC`

``` purescript
bindFC :: forall f g a. (Functor g) => FreeC f a -> Natural f (Free g) -> Free g a
```

Use a natural transformation to interpret a `FreeC` monad as a different
`Free` monad.

#### `injF`

``` purescript
injF :: forall f g a. (Functor f, Functor g, Inject f g) => Free f a -> Free g a
```

Embed computations in one `Free` monad as computations in the `Free` monad for
a coproduct type constructor.

This construction allows us to write computations which are polymorphic in the
particular `Free` monad we use, allowing us to extend the functionality of
our monad later.

#### `injFC`

``` purescript
injFC :: forall f g a. (Inject f g) => FreeC f a -> FreeC g a
```

Embed computations in one `FreeC` monad as computations in the `FreeC` monad for
a coproduct type constructor.

This construction allows us to write computations which are polymorphic in the
particular `Free` monad we use, allowing us to extend the functionality of
our monad later.

#### `runFree`

``` purescript
runFree :: forall f a. (Functor f) => (f (Free f a) -> Free f a) -> Free f a -> a
```

`runFree` runs a computation of type `Free f a`, using a function which unwraps a single layer of
the functor `f` at a time.

#### `runFreeM`

``` purescript
runFreeM :: forall f m a. (Functor f, MonadRec m) => (f (Free f a) -> m (Free f a)) -> Free f a -> m a
```

`runFreeM` runs a compuation of type `Free f a` in any `Monad` which supports tail recursion.
See the `MonadRec` type class for more details.

#### `runFreeC`

``` purescript
runFreeC :: forall f a. (forall a. f a -> a) -> FreeC f a -> a
```

`runFreeC` is the equivalent of `runFree` for type constructors transformed with `Coyoneda`,
hence we have no requirement that `f` be a `Functor`.

#### `runFreeCM`

``` purescript
runFreeCM :: forall f m a. (MonadRec m) => Natural f m -> FreeC f a -> m a
```

`runFreeCM` is the equivalent of `runFreeM` for type constructors transformed with `Coyoneda`,
hence we have no requirement that `f` be a `Functor`.


