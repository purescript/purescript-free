module Data.NaturalTransformation (NaturalTransformation(..)) where

-- | A natural transformation
type NaturalTransformation f g = forall a. f a -> g a
