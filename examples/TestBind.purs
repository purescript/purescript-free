module TestBind where

import Control.Monad.Eff
import Control.Monad.Trampoline

import Data.Array (range)
import Data.Foldable (foldl)

import Debug.Trace (Trace(), trace)

-- Tests derived from
-- https://github.com/mandubian/cats/tree/feature/freer

gen :: forall a. a -> Trampoline a
gen = suspend <<< done

leftBind :: forall f. Number -> Trampoline Number
leftBind n = foldl (\b a -> b >>= const (gen a)) (gen 0) (range 1 n)

rightBind :: forall f. Number -> Trampoline Number
rightBind n = foldl (\b a -> gen (n - a) >>= const b) (gen 0) (range 1 n)

foreign import now "function now(){ return new Date().valueOf(); }" :: forall eff. Eff eff Number

runner :: forall eff. Number -> Eff (trace :: Trace | eff) Unit
runner n = do
  t1 <- now
  pure $ runTrampoline $ leftBind n
  t2 <- now

  trace $ "leftBind: " ++ show (t2 - t1)

  t3 <- now
  pure $ runTrampoline $ rightBind n
  t4 <- now

  trace $ "rightBind: " ++ show (t4 - t3)

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
