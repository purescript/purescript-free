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

##### Instances
``` purescript
instance functorCofree :: (Functor f) => Functor (Cofree f)
instance foldableCofree :: (Foldable f) => Foldable (Cofree f)
instance traversableCofree :: (Traversable f) => Traversable (Cofree f)
instance extendCofree :: (Functor f) => Extend (Cofree f)
instance comonadCofree :: (Functor f) => Comonad (Cofree f)
instance applyCofree :: (Apply f) => Apply (Cofree f)
instance applicativeCofree :: (Applicative f) => Applicative (Cofree f)
instance bindCofree :: (MonadPlus f) => Bind (Cofree f)
instance monadCofree :: (MonadPlus f) => Monad (Cofree f)
```

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


