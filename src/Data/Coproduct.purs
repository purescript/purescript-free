module Data.Coproduct
  ( Coproduct(..)
  , coproduct
  ) where

data Coproduct f g a = Inl (f a) | Inr (g a)

instance coproductFunctor :: (Functor f, Functor g) => Functor (Coproduct f g) where
  (<$>) k (Inl fa) = Inl (k <$> fa)
  (<$>) k (Inr ga) = Inr (k <$> ga)

coproduct :: forall f g a b. (f a ->  b) -> (g a -> b) -> Coproduct f g a -> b
coproduct f _ (Inl x) = f x
coproduct _ g (Inr x) = g x
