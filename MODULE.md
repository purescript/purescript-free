# Module Documentation

## Module Control.Monad.Free

### Types

    data Free f a where
      Pure :: a -> Free f a
      Free :: f (Free f a) -> Free f a
      Gosub :: forall s. (forall r. (Unit -> Free f r) -> (r -> Free f a) -> s) -> s -> Free f a


### Type Classes

    class MonadFree f m where
      wrap :: forall a. f (m a) -> m a


### Type Class Instances

    instance applicativeFree :: (Functor f) => Applicative (Free f)

    instance applyFree :: (Functor f) => Apply (Free f)

    instance bindFree :: (Functor f) => Bind (Free f)

    instance functorFree :: (Functor f) => Functor (Free f)

    instance monadFree :: (Functor f) => Monad (Free f)

    instance monadFreeFree :: (Functor f) => MonadFree f (Free f)

    instance monadTransFree :: MonadTrans Free


### Values

    go :: forall f a. (Functor f) => (f (Free f a) -> Free f a) -> Free f a -> a

    goEff :: forall e f a. (Functor f) => (f (Free f a) -> Eff e (Free f a)) -> Free f a -> Eff e a

    goM :: forall f m a. (Functor f, Monad m) => (f (Free f a) -> m (Free f a)) -> Free f a -> m a

    iterM :: forall f m a. (Functor f, Monad m) => (forall a. f (m a) -> m a) -> Free f a -> m a

    liftF :: forall f m a. (Functor f, Monad m, MonadFree f m) => f a -> m a

    pureF :: forall f a. (Applicative f) => a -> Free f a


## Module Control.Monad.Trampoline

### Types

    newtype Delay a where
      Delay :: Unit -> a -> Delay a

    type Trampoline a = Free Delay a


### Type Class Instances

    instance delayApplicative :: Applicative Delay

    instance delayApply :: Apply Delay

    instance delayFunctor :: Functor Delay


### Values

    delay :: forall a. (Unit -> a) -> Trampoline a

    done :: forall a. a -> Trampoline a

    runTrampoline :: forall a. Trampoline a -> a

    suspend :: forall a. Trampoline a -> Trampoline a



