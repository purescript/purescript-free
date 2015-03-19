# Module Documentation

## Module Control.Comonad.Cofree


The _cofree comonad_ for a `Functor`.

#### `Cofree`

``` purescript
data Cofree f a
```

The `Cofree` `Comonad` for a functor.

A value of type `Cofree f a` consists of an `f`-branching
tree, annotated with labels of type `a`.

The `Comonad` instance supports _redecoration_, recomputing
labels from the local context.

#### `mkCofree`

``` purescript
mkCofree :: forall f a. a -> f (Cofree f a) -> Cofree f a
```

Create a value of type `Cofree f a` from a label and a
functor-full of "subtrees".

#### `head`

``` purescript
head :: forall f a. Cofree f a -> a
```

Returns the label for a tree.

#### `tail`

``` purescript
tail :: forall f a. Cofree f a -> f (Cofree f a)
```

Returns the "subtrees" of a tree.

#### `functorCofree`

``` purescript
instance functorCofree :: (Functor f) => Functor (Cofree f)
```


#### `foldableCofree`

``` purescript
instance foldableCofree :: (Foldable f) => Foldable (Cofree f)
```


#### `traversableCofree`

``` purescript
instance traversableCofree :: (Traversable f) => Traversable (Cofree f)
```


#### `extendCofree`

``` purescript
instance extendCofree :: (Functor f) => Extend (Cofree f)
```


#### `comonadCofree`

``` purescript
instance comonadCofree :: (Functor f) => Comonad (Cofree f)
```


#### `applyCofree`

``` purescript
instance applyCofree :: (Apply f) => Apply (Cofree f)
```


#### `applicativeCofree`

``` purescript
instance applicativeCofree :: (Applicative f) => Applicative (Cofree f)
```


#### `bindCofree`

``` purescript
instance bindCofree :: (MonadPlus f) => Bind (Cofree f)
```


#### `monadCofree`

``` purescript
instance monadCofree :: (MonadPlus f) => Monad (Cofree f)
```



## Module Control.Monad.Free

#### `Free`

``` purescript
data Free f a
  = Pure a
  | Free (f (Free f a))
  | Gosub (forall s. (forall r. (Unit -> Free f r) -> (r -> Free f a) -> s) -> s)
```

The free `Monad` for a `Functor`.

The implementation defers the evaluation of monadic binds so that it
is safe to use monadic tail recursion, for example.

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

#### `functorFree`

``` purescript
instance functorFree :: (Functor f) => Functor (Free f)
```


#### `applyFree`

``` purescript
instance applyFree :: (Functor f) => Apply (Free f)
```


#### `applicativeFree`

``` purescript
instance applicativeFree :: (Functor f) => Applicative (Free f)
```


#### `bindFree`

``` purescript
instance bindFree :: (Functor f) => Bind (Free f)
```


#### `monadFree`

``` purescript
instance monadFree :: (Functor f) => Monad (Free f)
```


#### `monadTransFree`

``` purescript
instance monadTransFree :: MonadTrans Free
```


#### `monadFreeFree`

``` purescript
instance monadFreeFree :: (Functor f) => MonadFree f (Free f)
```


#### `liftF`

``` purescript
liftF :: forall f m a. (Functor f, Monad m, MonadFree f m) => f a -> m a
```

Lift an action described by the generating functor `f` into the monad `m`
(usually `Free f`).

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

#### `injC`

``` purescript
injC :: forall f g a. (Inject f g) => FreeC f a -> FreeC g a
```

Embed computations in one `Free` monad as computations in the `Free` monad for
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


## Module Control.Monad.Trampoline


A _trampoline_ monad, which can be used at the bottom of
a monad transformer stack to avoid stack overflows in large
monadic computations.

#### `Trampoline`

``` purescript
type Trampoline = Free Lazy
```

The `Trampoline` monad

A computation of type `Trampoline a` consists of zero or more lazy 
suspensions before a value is returned.

#### `done`

``` purescript
done :: forall a. a -> Trampoline a
```

Return a value immediately

#### `suspend`

``` purescript
suspend :: forall a. Trampoline a -> Trampoline a
```

Suspend a computation by one step.

#### `delay'`

``` purescript
delay' :: forall a. Lazy a -> Trampoline a
```

Use the `Trampoline` monad to represent a `Lazy` value.

#### `delay`

``` purescript
delay :: forall a. (Unit -> a) -> Trampoline a
```

Use the `Trampoline` monad to represent the delayed evaluation of a value.

#### `runTrampoline`

``` purescript
runTrampoline :: forall a. Trampoline a -> a
```

Run a computation in the `Trampoline` monad.


## Module Data.Coyoneda

#### `CoyonedaF`

``` purescript
newtype CoyonedaF f a i
  = CoyonedaF { fi :: f i, k :: i -> a }
```

`Coyoneda` is encoded as an existential type using `Data.Exists`.

This type constructor encodes the contents of the existential package.

#### `Coyoneda`

``` purescript
newtype Coyoneda f a
  = Coyoneda (Exists (CoyonedaF f a))
```

The `Coyoneda` `Functor`.

`Coyoneda f` is a `Functor` for any type constructor `f`. In fact,
it is the _free_ `Functor` for `f`.

#### `Natural`

``` purescript
type Natural f g = forall a. f a -> g a
```

A natural transformation

#### `functorCoyoneda`

``` purescript
instance functorCoyoneda :: Functor (Coyoneda f)
```


#### `applyCoyoneda`

``` purescript
instance applyCoyoneda :: (Apply f) => Apply (Coyoneda f)
```


#### `applicativeCoyoneda`

``` purescript
instance applicativeCoyoneda :: (Applicative f) => Applicative (Coyoneda f)
```


#### `bindCoyoneda`

``` purescript
instance bindCoyoneda :: (Bind f) => Bind (Coyoneda f)
```


#### `monadCoyoneda`

``` purescript
instance monadCoyoneda :: (Monad f) => Monad (Coyoneda f)
```


#### `monadTransCoyoneda`

``` purescript
instance monadTransCoyoneda :: MonadTrans Coyoneda
```


#### `extendCoyoneda`

``` purescript
instance extendCoyoneda :: (Extend w) => Extend (Coyoneda w)
```


#### `comonadCoyoneda`

``` purescript
instance comonadCoyoneda :: (Comonad w) => Comonad (Coyoneda w)
```


#### `coyoneda`

``` purescript
coyoneda :: forall f a b. (a -> b) -> f a -> Coyoneda f b
```

Construct a value of type `Coyoneda f a`.

#### `liftCoyoneda`

``` purescript
liftCoyoneda :: forall f a. f a -> Coyoneda f a
```

Lift a value described by the type constructor `f` to `Coyoneda f`.

#### `lowerCoyoneda`

``` purescript
lowerCoyoneda :: forall f a. (Functor f) => Coyoneda f a -> f a
```

Lower a value of type `Yoneda f a` to the `Functor` `f`. 

#### `liftCoyonedaT`

``` purescript
liftCoyonedaT :: forall f g. Natural f g -> Natural (Coyoneda f) (Coyoneda g)
```


#### `liftCoyonedaTF`

``` purescript
liftCoyonedaTF :: forall f g. (Functor g) => Natural f g -> Natural (Coyoneda f) g
```



## Module Data.Yoneda

#### `Yoneda`

``` purescript
newtype Yoneda f a
  = Yoneda (forall b. (a -> b) -> f b)
```

The Yoneda `Functor`

`Yoneda f` is a `Functor` for any type constructor `f`.

#### `functorYoneda`

``` purescript
instance functorYoneda :: Functor (Yoneda f)
```


#### `applyYoneda`

``` purescript
instance applyYoneda :: (Apply f) => Apply (Yoneda f)
```


#### `applicativeYoneda`

``` purescript
instance applicativeYoneda :: (Applicative f) => Applicative (Yoneda f)
```


#### `bindCoyoneda`

``` purescript
instance bindCoyoneda :: (Bind f) => Bind (Yoneda f)
```


#### `monadYoneda`

``` purescript
instance monadYoneda :: (Monad f) => Monad (Yoneda f)
```


#### `monadTransYoneda`

``` purescript
instance monadTransYoneda :: MonadTrans Yoneda
```


#### `extendYoneda`

``` purescript
instance extendYoneda :: (Extend w) => Extend (Yoneda w)
```


#### `comonadYoneda`

``` purescript
instance comonadYoneda :: (Comonad w) => Comonad (Yoneda w)
```


#### `runYoneda`

``` purescript
runYoneda :: forall f a b. Yoneda f a -> (a -> b) -> f b
```

Run a computation of type `Yoneda f a`.

#### `liftYoneda`

``` purescript
liftYoneda :: forall f a. (Functor f) => f a -> Yoneda f a
```

Lift a value described by the `Functor` `f` to the
`Functor` `Yoneda f`.

#### `lowerYoneda`

``` purescript
lowerYoneda :: forall f a. Yoneda f a -> f a
```

Lower a value of type `Yoneda f a` to the type constructor `f`.