# Module Documentation

## Module Control.Monad.Free

### Types

    data Free f a where
      Return :: a -> Free f a
      Suspend :: f (Free f a) -> Free f a
      Gosub :: forall s. (forall r. ({} -> Free f r) -> (r -> Free f a) -> s) -> s -> Free f a


### Type Classes

    class MonadFree f m where
      wrap :: forall a. f (m a) -> m a


### Type Class Instances

    instance applicativeFree :: (Functor f) => Applicative (Free f)

    instance applyFree :: (Functor f) => Apply (Free f)

    instance bindFree :: (Functor f) => Bind (Free f)

    instance functorFree :: (Functor f) => Functor (Free f)

    instance monadFree :: (Functor f) => Monad (Free f)

    instance monadTransFree :: MonadTrans Free

    instance monadFreeFree :: (Functor f) => MonadFree f (Free f)


### Values

    bindFree :: forall f a b. (a -> Free f b) -> Free f a -> Free f b

    go :: forall f a. (Functor f) => (f (Free f a) -> Free f a) -> Free f a -> a

    liftF :: forall f a. (Functor f) => f a -> Free f a

    pureF :: forall f a. (Applicative f) => a -> Free f a

    -- Note: can blow the stack!
    iterM :: forall f m a. (Functor f, Monad m) => (f (m a) -> m a) -> Free f a -> m a

    resume :: forall f a. (Functor f) => Free f a -> Either (f (Free f a)) a

    resumeGosub :: forall f a. (Functor f) => (forall s. (forall r. ({  } -> Free f r) -> (r -> Free f a) -> s) -> s) -> Either (f (Free f a)) (Free f a)


## Module Control.Monad.Trampoline

### Types

    data Delay a where
      Delay :: {  } -> a -> Delay a

    type Trampoline a = Free Delay a


### Type Class Instances

    instance delayApplicative :: Applicative Delay

    instance delayApply :: Apply Delay

    instance delayFunctor :: Functor Delay


### Values

    delay :: forall a. ({  } -> a) -> Trampoline a

    done :: forall a. a -> Trampoline a

    runTrampoline :: forall a. Trampoline a -> a

    suspend :: forall a. Trampoline a -> Trampoline a
