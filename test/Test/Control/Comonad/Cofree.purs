module Test.Control.Comonad.Cofree where

import Prelude

import Control.Comonad.Cofree (Cofree, head, mkCofree, tail)
import Control.Comonad.ZipCofree (ZipCofree(..))
import Data.Maybe (Maybe(..))
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
