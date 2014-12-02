# Module Documentation

## Module Control.Comonad.Cofree

### Types

    data Cofree f a


### Type Class Instances

    instance applicativeCofree :: (Applicative f) => Applicative (Cofree f)

    instance applyCofree :: (Apply f) => Apply (Cofree f)

    instance bindCofree :: (MonadPlus f) => Bind (Cofree f)

    instance comonadCofree :: (Functor f) => Comonad (Cofree f)

    instance extendCofree :: (Functor f) => Extend (Cofree f)

    instance foldableCofree :: (Foldable f) => Foldable (Cofree f)

    instance functorCofree :: (Functor f) => Functor (Cofree f)

    instance monadCofree :: (MonadPlus f) => Monad (Cofree f)

    instance traversableCofree :: (Traversable f) => Traversable (Cofree f)


### Values

    head :: forall f a. Cofree f a -> a

    mkCofree :: forall f a. a -> f (Cofree f a) -> Cofree f a

    tail :: forall f a. Cofree f a -> f (Cofree f a)


## Module Control.Monad.Free

### Types

    data Free f a where
      Pure :: a -> Free f a
      Free :: f (Free f a) -> Free f a
      Gosub :: (forall s. (forall r. (Unit -> Free f r) -> (r -> Free f a) -> s) -> s) -> Free f a

    type FreeC f = Free (Coyoneda f)


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

    goEffC :: forall e f a. Natural f (Eff e) -> FreeC f a -> Eff e a

    goM :: forall f m a. (Functor f, Monad m) => (f (Free f a) -> m (Free f a)) -> Free f a -> m a

    goMC :: forall f m a. (Monad m) => Natural f m -> FreeC f a -> m a

    injC :: forall f g a. (Inject f g) => FreeC f a -> FreeC g a

    iterM :: forall f m a. (Functor f, Monad m) => (forall a. f (m a) -> m a) -> Free f a -> m a

    liftF :: forall f m a. (Functor f, Monad m, MonadFree f m) => f a -> m a

    liftFC :: forall f a. f a -> FreeC f a

    mapF :: forall f g a. (Functor f, Functor g) => Natural f g -> Free f a -> Free g a

    pureF :: forall f a. (Applicative f) => a -> Free f a

    pureFC :: forall f a. (Applicative f) => a -> FreeC f a


## Module Control.Monad.Trampoline

### Types

    type Trampoline = Free Lazy


### Values

    delay :: forall a. (Unit -> a) -> Trampoline a

    delay' :: forall a. Lazy a -> Trampoline a

    done :: forall a. a -> Trampoline a

    runTrampoline :: forall a. Trampoline a -> a

    suspend :: forall a. Trampoline a -> Trampoline a


## Module Data.Coyoneda

### Types

    newtype Coyoneda f a where
      Coyoneda :: Exists (CoyonedaF f a) -> Coyoneda f a

    newtype CoyonedaF f a i where
      CoyonedaF :: { fi :: f i, k :: i -> a } -> CoyonedaF f a i

    type Natural f g = forall a. f a -> g a


### Type Class Instances

    instance applicativeCoyoneda :: (Applicative f) => Applicative (Coyoneda f)

    instance applyCoyoneda :: (Apply f) => Apply (Coyoneda f)

    instance bindCoyoneda :: (Bind f) => Bind (Coyoneda f)

    instance comonadCoyoneda :: (Comonad w) => Comonad (Coyoneda w)

    instance extendCoyoneda :: (Extend w) => Extend (Coyoneda w)

    instance functorCoyoneda :: Functor (Coyoneda f)

    instance monadCoyoneda :: (Monad f) => Monad (Coyoneda f)

    instance monadTransCoyoneda :: MonadTrans Coyoneda


### Values

    coyoneda :: forall f a b. (a -> b) -> f a -> Coyoneda f b

    liftCoyoneda :: forall f a. f a -> Coyoneda f a

    liftCoyonedaT :: forall f g. Natural f g -> Natural (Coyoneda f) (Coyoneda g)

    liftCoyonedaTF :: forall f g. (Functor g) => Natural f g -> Natural (Coyoneda f) g

    lowerCoyoneda :: forall f a. (Functor f) => Coyoneda f a -> f a


## Module Data.Yoneda

### Types

    newtype Yoneda f a where
      Yoneda :: (forall b. (a -> b) -> f b) -> Yoneda f a


### Type Class Instances

    instance applicativeYoneda :: (Applicative f) => Applicative (Yoneda f)

    instance applyYoneda :: (Apply f) => Apply (Yoneda f)

    instance bindCoyoneda :: (Bind f) => Bind (Yoneda f)

    instance comonadYoneda :: (Comonad w) => Comonad (Yoneda w)

    instance extendYoneda :: (Extend w) => Extend (Yoneda w)

    instance functorYoneda :: Functor (Yoneda f)

    instance monadTransYoneda :: MonadTrans Yoneda

    instance monadYoneda :: (Monad f) => Monad (Yoneda f)


### Values

    liftYoneda :: forall f a. (Functor f) => f a -> Yoneda f a

    lowerYoneda :: forall f a. Yoneda f a -> f a

    runYoneda :: forall f a b. Yoneda f a -> (a -> b) -> f b