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


