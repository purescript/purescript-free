# Module Documentation

## Module Control.Comonad.Cofree

#### `Cofree`

``` purescript
data Cofree f a
```


#### `mkCofree`

``` purescript
mkCofree :: forall f a. a -> f (Cofree f a) -> Cofree f a
```


#### `head`

``` purescript
head :: forall f a. Cofree f a -> a
```


#### `tail`

``` purescript
tail :: forall f a. Cofree f a -> f (Cofree f a)
```


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


#### `FreeC`

``` purescript
type FreeC f = Free (Coyoneda f)
```


#### `MonadFree`

``` purescript
class MonadFree f m where
  wrap :: forall a. f (m a) -> m a
```


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


#### `pureF`

``` purescript
pureF :: forall f a. (Applicative f) => a -> Free f a
```


#### `liftFC`

``` purescript
liftFC :: forall f a. f a -> FreeC f a
```


#### `pureFC`

``` purescript
pureFC :: forall f a. (Applicative f) => a -> FreeC f a
```


#### `mapF`

``` purescript
mapF :: forall f g a. (Functor f, Functor g) => Natural f g -> Free f a -> Free g a
```


#### `injC`

``` purescript
injC :: forall f g a. (Inject f g) => FreeC f a -> FreeC g a
```


#### `iterM`

``` purescript
iterM :: forall f m a. (Functor f, Monad m) => (forall a. f (m a) -> m a) -> Free f a -> m a
```

Note: can blow the stack!

#### `go`

``` purescript
go :: forall f a. (Functor f) => (f (Free f a) -> Free f a) -> Free f a -> a
```

`go` runs a computation of type `Free f a`, using a function which unwraps a single layer of
the functor `f` at a time.

#### `goM`

``` purescript
goM :: forall f m a. (Functor f, MonadRec m) => (f (Free f a) -> m (Free f a)) -> Free f a -> m a
```

`goM` runs a compuation of type `Free f a` in any `Monad` which supports tail recursion.
See the `MonadRec` type class for more details.

#### `goEff`

``` purescript
goEff :: forall e f a. (Functor f) => (f (Free f a) -> Eff e (Free f a)) -> Free f a -> Eff e a
```

`goEff` is `goM` specialized to the `Eff` monad.

#### `goMC`

``` purescript
goMC :: forall f m a. (MonadRec m) => Natural f m -> FreeC f a -> m a
```

`goMC` is the equivalent of `goM` for type constructors transformed with `Coyoneda`,
hence we have no requirement that `f` be a `Functor`.

#### `goEffC`

``` purescript
goEffC :: forall e f a. Natural f (Eff e) -> FreeC f a -> Eff e a
```

`goEffC` is `goMC` specialized to the `Eff` monad.


## Module Control.Monad.Trampoline

#### `Trampoline`

``` purescript
type Trampoline = Free Lazy
```


#### `done`

``` purescript
done :: forall a. a -> Trampoline a
```


#### `suspend`

``` purescript
suspend :: forall a. Trampoline a -> Trampoline a
```


#### `delay'`

``` purescript
delay' :: forall a. Lazy a -> Trampoline a
```


#### `delay`

``` purescript
delay :: forall a. (Unit -> a) -> Trampoline a
```


#### `runTrampoline`

``` purescript
runTrampoline :: forall a. Trampoline a -> a
```



## Module Data.Coyoneda

#### `CoyonedaF`

``` purescript
newtype CoyonedaF f a i
  = CoyonedaF { fi :: f i, k :: i -> a }
```


#### `Coyoneda`

``` purescript
newtype Coyoneda f a
  = Coyoneda (Exists (CoyonedaF f a))
```


#### `Natural`

``` purescript
type Natural f g = forall a. f a -> g a
```


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


#### `liftCoyoneda`

``` purescript
liftCoyoneda :: forall f a. f a -> Coyoneda f a
```


#### `lowerCoyoneda`

``` purescript
lowerCoyoneda :: forall f a. (Functor f) => Coyoneda f a -> f a
```


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


#### `liftYoneda`

``` purescript
liftYoneda :: forall f a. (Functor f) => f a -> Yoneda f a
```


#### `lowerYoneda`

``` purescript
lowerYoneda :: forall f a. Yoneda f a -> f a
```