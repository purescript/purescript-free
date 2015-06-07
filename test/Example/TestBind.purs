module Example.TestBind where

import Prelude

import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Trampoline
import Data.Array (range)
import Data.Foldable (foldl)

-- Tests derived from
-- https://github.com/mandubian/cats/tree/feature/freer

gen :: forall a. a -> Trampoline a
gen = suspend <<< done

leftBind :: forall f. Int -> Trampoline Int
leftBind n = foldl (\b a -> b >>= const (gen a)) (gen 0) (range 1 n)

rightBind :: forall f. Int -> Trampoline Int
rightBind n = foldl (\b a -> gen (n - a) >>= const b) (gen 0) (range 1 n)

foreign import now :: forall eff. Eff eff Int

runner :: forall eff. Int -> Eff (console :: CONSOLE | eff) Unit
runner n = do
  t1 <- now
  pure $ runTrampoline $ leftBind n
  t2 <- now

  log $ "leftBind: " ++ show (t2 - t1)

  t3 <- now
  pure $ runTrampoline $ rightBind n
  t4 <- now

  log $ "rightBind: " ++ show (t4 - t3)

  return unit

main = do
  runner 10000
  runner 20000
  runner 30000
  runner 40000
  runner 50000
  runner 60000
  runner 70000
  runner 80000
  runner 90000
  runner 100000
