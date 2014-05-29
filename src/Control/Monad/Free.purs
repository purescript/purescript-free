module Control.Monad.Free where

import Control.Monad.Trans
import Data.Either

data Free f a = Pure a
              | Free (f (Free f a))
              | Gosub (forall s. (forall r. ({} -> Free f r) -> (r -> Free f a) -> s) -> s)

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
iterM k (Gosub f) = f (\req recv -> iterM k (req {}) >>= (iterM k <<< recv))

runM :: forall f m a. (Functor f, Monad m) => (f (Free f a) -> m (Free f a)) -> Free f a -> m a
runM k f = case resume f of
             Left s -> k s >>= runM k
             Right a -> return a

foldMap :: forall f m a. (Functor f, Monad m) => (forall r. f r -> m r) -> Free f a -> m a
foldMap k f = case resume f of
                Left s -> k s >>= foldMap k
                Right a -> return a

resumeGosub :: forall f a. (Functor f) => (forall s. (forall r. ({} -> Free f r) -> (r -> Free f a) -> s) -> s) -> Either (f (Free f a)) (Free f a)
resumeGosub f = f (\a g ->
  case a {} of
    Pure a -> Right (g a)
    Free t -> Left ((\h -> h >>= g) <$> t)
    Gosub h -> Right (h (\b i -> b {} >>= (\x -> i x >>= g)))
  )

foreign import resume
  "function resume(__dict_Functor) {\
  \  return function(__copy__1) {\
  \    var _1 = __copy__1;\
  \    tco: while (true)\
  \      if (_1.ctor === 'Control.Monad.Free.Pure')\
  \        return Data_Either.Right(_1.values[0]);\
  \      else if (_1.ctor === 'Control.Monad.Free.Free')\
  \        return Data_Either.Left(_1.values[0]);\
  \      else {\
  \        var x = resumeGosub(__dict_Functor)(_1.values[0]);\
  \        if (x.ctor === 'Data.Either.Left')\
  \          return x;\
  \        else {\
  \          _1 = x.values[0];\
  \          continue tco;\
  \        }\
  \      }\
  \  };\
  \}" :: forall f a. (Functor f) => Free f a -> Either (f (Free f a)) a

foreign import go
  "function go(__dict_Functor) {\
  \  return function(f) {\
  \    return function(__copy__1) {\
  \      var _1 = __copy__1;\
  \      var r;\
  \      tco: while (true) {\
  \        r = resume(__dict_Functor)(_1);\
  \        if (r.ctor === 'Data.Either.Left') {\
  \          _1 = f(r.values[0]);\
  \          continue tco;\
  \        } else\
  \          return r.values[0];\
  \      }\
  \    };\
  \  };\
  \}" :: forall f a. (Functor f) => (f (Free f a) -> Free f a) -> Free f a -> a
