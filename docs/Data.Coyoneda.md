## Module Data.Coyoneda

#### `CoyonedaF`

``` purescript
newtype CoyonedaF f a i
  = CoyonedaF { k :: i -> a, fi :: f i }
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

##### Instances
``` purescript
instance functorCoyoneda :: Functor (Coyoneda f)
instance applyCoyoneda :: (Apply f) => Apply (Coyoneda f)
instance applicativeCoyoneda :: (Applicative f) => Applicative (Coyoneda f)
instance bindCoyoneda :: (Bind f) => Bind (Coyoneda f)
instance monadCoyoneda :: (Monad f) => Monad (Coyoneda f)
instance monadTransCoyoneda :: MonadTrans Coyoneda
instance extendCoyoneda :: (Extend w) => Extend (Coyoneda w)
instance comonadCoyoneda :: (Comonad w) => Comonad (Coyoneda w)
```

#### `Natural`

``` purescript
type Natural f g = forall a. f a -> g a
```

A natural transformation

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


