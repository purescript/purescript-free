module Test.Control.Comonad.Cofree where

import Prelude

import Control.Comonad.Cofree (Cofree, buildCofree, head, hoistCofree, mkCofree, recastCofree, tail)
import Control.Comonad.ZipCofree (ZipCofree(..))
import Data.Foldable (foldr)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Exception (throw)

labels :: forall a. Int -> Cofree Maybe a -> Array a
labels depth tree
  | depth <= 0 = []
  | otherwise = [ head tree ] <> case tail tree of
      Nothing -> []
      Just child -> labels (depth - 1) child

main :: Effect Unit
main = do
  let
    values = mkCofree 2 (Just (mkCofree 5 (Just (mkCofree 9 Nothing))))
    functions = mkCofree (_ + 10) (Just (mkCofree (_ * 3) Nothing))
    ZipCofree zipped = ZipCofree functions <*> ZipCofree values
    ZipCofree shorter = ZipCofree functions <*> ZipCofree (mkCofree 7 Nothing)
    ZipCofree repeated = pure 11 :: ZipCofree Maybe Int
    ZipCofree unchanged = pure identity <*> ZipCofree values
  unless (labels 4 zipped == [ 12, 15 ]) do
    throw "ZipCofree must apply functions layer-wise and stop when functions end"
  unless (labels 4 shorter == [ 17 ]) do
    throw "ZipCofree must stop when values end"
  unless (labels 4 repeated == [ 11, 11, 11, 11 ]) do
    throw "ZipCofree pure must lazily repeat at every layer"
  unless (unchanged == values) do
    throw "ZipCofree must satisfy applicative identity"

  let
    tree = mkCofree 0
      [ mkCofree 3 []
      , mkCofree 8 [ mkCofree 4 [], mkCofree 12 [] ]
      ]

    choose :: forall z. (Cofree Array Int -> z) -> Array (Cofree Array Int) -> Maybe z
    choose convert = foldr (\child rest -> if head child >= 8 then Just (convert child) else rest) Nothing
    pruned = recastCofree choose tree

    toArray :: Maybe ~> Array
    toArray Nothing = []
    toArray (Just child) = [ child ]
    expected = mkCofree 2 [ mkCofree 5 [ mkCofree 9 [] ] ]
    infinite = buildCofree (\n -> Tuple n (Just (n + 1))) 1
  unless (labels 4 pruned == [ 0, 8, 12 ]) do
    throw "recastCofree must inspect original subtree labels and recurse into selected branches"
  unless (hoistCofree toArray values == expected) do
    throw "hoistCofree must preserve labels and transform every layer"
  unless (labels 4 (recastCofree map infinite) == [ 1, 2, 3, 4 ]) do
    throw "recastCofree must preserve laziness"
  unless (labels 4 (hoistCofree identity infinite) == [ 1, 2, 3, 4 ]) do
    throw "hoistCofree must preserve laziness"
