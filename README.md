# Module Documentation

## Module Data.Coyoneda

### Types

    newtype Coyoneda f a where
      Coyoneda :: Exists (CoyonedaF f a) -> Coyoneda f a

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
      Yoneda :: forall b. (a -> b) -> f b -> Yoneda f a


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



