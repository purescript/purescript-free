module Data.NaturalTransformation
  ( NaturalTransformation(..)
  , Natural(..)
  ) where

-- | A natural transformation
type NaturalTransformation f g = forall a. f a -> g a

-- | Alias for `NaturalTransformation`
type Natural f g = NaturalTransformation f g
