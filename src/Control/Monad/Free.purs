module Control.Monad.Free
  ( Free(..)
  , MonadFree, wrap
  , liftF
  , pureF
  , iterM
  , goM
  , go
  , goEff
  ) where

import Control.Monad.Trans
import Control.Monad.Eff
import Data.Either
import Data.Function

data Free f a = Pure a
              | Free (f (Free f a))
              | Gosub (forall s. (forall r. (Unit -> Free f r) -> (r -> Free f a) -> s) -> s)

class MonadFree f m where
  wrap :: forall a. f (m a) -> m a

instance functorFree :: (Functor f) => Functor (Free f) where
  (<$>) f (Pure a) = Pure (f a)
  (<$>) f g = liftA1 f g

instance applyFree :: (Functor f) => Apply (Free f) where
  (<*>) = ap

instance applicativeFree :: (Functor f) => Applicative (Free f) where
  pure = Pure

instance bindFree :: (Functor f) => Bind (Free f) where
  (>>=) (Gosub g) f = Gosub (\h -> g (\a i -> h a (\x -> Gosub (\j -> j (const (i x)) f))))
  (>>=) a         f = Gosub (\h -> h (const a) f)

instance monadFree :: (Functor f) => Monad (Free f)

instance monadTransFree :: MonadTrans Free where
  lift f = Free $ do
    a <- f
    return (Pure a)

instance monadFreeFree :: (Functor f) => MonadFree f (Free f) where
  wrap = Free

liftF :: forall f m a. (Functor f, Monad m, MonadFree f m) => f a -> m a
liftF fa = wrap $ return <$> fa

pureF :: forall f a. (Applicative f) => a -> Free f a
pureF a = Free (pure (Pure  a))

-- Note: can blow the stack!
iterM :: forall f m a. (Functor f, Monad m) => (forall a. f (m a) -> m a) -> Free f a -> m a
iterM _ (Pure a) = return a
iterM k (Free f) = k $ iterM k <$> f
iterM k (Gosub f) = f (\req recv -> iterM k (req unit) >>= (iterM k <<< recv))

-- Note: can blow the stack!
goM :: forall f m a. (Functor f, Monad m) => (f (Free f a) -> m (Free f a)) -> Free f a -> m a
goM k f = case resume f of
            Left s -> k s >>= goM k
            Right a -> return a

resumeGosub :: forall f a. (Functor f) => Free f a -> Either (f (Free f a)) (Free f a)
resumeGosub (Gosub f) = f (\a g ->
  case a unit of
    Pure a -> Right (g a)
    Free t -> Left ((\h -> h >>= g) <$> t)
    Gosub h -> Right (h (\b i -> b unit >>= (\x -> i x >>= g)))
  )

isGosub :: forall f a. Free f a -> Boolean
isGosub (Gosub _) = true
isGosub _ = false

unsafeFreeToEither :: forall f a. Free f a -> Either (f (Free f a)) a
unsafeFreeToEither (Pure x) = Right x
unsafeFreeToEither (Free x) = Left x

unsafeLeft :: forall a b. Either a b -> a
unsafeLeft (Left x) = x

unsafeRight :: forall a b. Either a b -> b
unsafeRight (Right x) = x

foreign import resumeImpl
  "function resumeImpl(isGosub, isLeft, toEither, fromRight, resumeGosub, value) {\
  \  while (true) {\
  \    if (!isGosub(value)) return toEither(value);\
  \    var x = resumeGosub(value);\
  \    if (isLeft(x)) return x;\
  \    else value = fromRight(x);\
  \  }\
  \}" :: forall f a. Fn6
         (Free f a -> Boolean)
         (Either (f (Free f a)) a -> Boolean)
         (Free f a -> Either (f (Free f a)) a)
         (Either (f (Free f a)) a -> a)
         (Free f a -> Either (f (Free f a)) (Free f a))
         (Free f a)
         (Either (f (Free f a)) a)

resume :: forall f a. (Functor f) => Free f a -> Either (f (Free f a)) a
resume f = runFn6 resumeImpl isGosub isLeft unsafeFreeToEither unsafeRight resumeGosub f

foreign import goImpl
  "function goImpl(resume, isRight, fromLeft, fromRight, fn, value) {\
  \  while (true) {\
  \    var r = resume(value);\
  \    if (isRight(r)) return fromRight(r);\
  \    value = fn(fromLeft(r));\
  \  }\
  \}" :: forall f a. Fn6
         (Free f a -> Either (f (Free f a)) a)
         (Either (f (Free f a)) a -> Boolean)
         (Either (f (Free f a)) a -> (f (Free f a)))
         (Either (f (Free f a)) a -> a)
         (f (Free f a) -> Free f a)
         (Free f a)
         a

go :: forall f a. (Functor f) => (f (Free f a) -> Free f a) -> Free f a -> a
go fn f = runFn6 goImpl resume isRight unsafeLeft unsafeRight fn f

foreign import goEffImpl
  "function goEffImpl(resume, isRight, fromLeft, fromRight, fn, value) {\
  \  return function(){\
  \    while (true) {\
  \      var r = resume(value);\
  \      if (isRight(r)) {\
  \        var x = fromRight(r);\
  \        return function() { return x; };\
  \      }\
  \      value = fn(fromLeft(r))();\
  \    }\
  \  };\
  \}" :: forall e f a. Fn6
         (Free f a -> Either (f (Free f a)) a)
         (Either (f (Free f a)) a -> Boolean)
         (Either (f (Free f a)) a -> (f (Free f a)))
         (Either (f (Free f a)) a -> a)
         (f (Free f a) -> Eff e (Free f a))
         (Free f a)
         (Eff e a)

goEff :: forall e f a. (Functor f) => (f (Free f a) -> Eff e (Free f a)) -> Free f a -> Eff e a
goEff fn f = runFn6 goEffImpl resume isRight unsafeLeft unsafeRight fn f
