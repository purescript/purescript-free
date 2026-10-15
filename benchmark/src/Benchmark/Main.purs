module Benchmark.Main (main) where

import Prelude

import Benchmark.Freef686f5f as Freef686f5f
import Benchmark.Harness (Benchmark, benchFn, mkBenchmark, runSuite)
import Benchmark.Trampoline0df59c5 as Trampoline0df59c5
import Benchmark.Trampolinef686f5f as Trampolinef686f5f
import Data.Array (replicate)
import Data.Foldable (foldl)
import Effect (Effect)

main :: Effect Unit
main =
  runSuite
    [ leftBindSmallBenchmark
    , rightBindSmallBenchmark
    , leftBindLargeBenchmark
    , rightBindLargeBenchmark
    ]

leftBindSmallBenchmark :: Benchmark
leftBindSmallBenchmark =
  mkBenchmark
    { slug: "left-bind-small"
    , title: "Left associated binds (small - " <> show inputsPerSize <> " inputs per size)"
    , sizes: [ 1, 2, 3, 4, 5, 10, 20, 30, 40, 50, 100, 250, 500, 1000 ]
    , sizeInterpretation: "Number of binds"
    , inputsPerSize: inputsPerSize
    , gen: \n -> replicate n 0.0
    , functions:
        [ benchFn "Free v5.2.0" (Trampolinef686f5f.runTrampoline <<< binds)
        , benchFn "Free v0.6.1" (Trampoline0df59c5.runTrampoline <<< bindsT)
        ]
    }
  where
  inputsPerSize :: Int
  inputsPerSize = 100

  binds :: Array Number -> Trampolinef686f5f.Trampoline Number
  binds as = foldl (\b a -> b >>= const (gen a)) (gen 0.0) as

  gen :: forall a. a -> Trampolinef686f5f.Trampoline a
  gen = Freef686f5f.suspendF <<< Trampolinef686f5f.done

  bindsT :: Array Number -> Trampoline0df59c5.Trampoline Number
  bindsT as = foldl (\b a -> b >>= const (genT a)) (genT 0.0) as

  genT :: forall a. a -> Trampoline0df59c5.Trampoline a
  genT = Trampoline0df59c5.suspend <<< Trampoline0df59c5.done

rightBindSmallBenchmark :: Benchmark
rightBindSmallBenchmark =
  mkBenchmark
    { slug: "right-bind-small"
    , title: "Right associated binds (small - " <> show inputsPerSize <> " inputs per size)"
    , sizes: [ 1, 2, 3, 4, 5, 10, 20, 30, 40, 50, 100, 250, 500, 1000 ]
    , sizeInterpretation: "Number of binds"
    , inputsPerSize: inputsPerSize
    , gen: \n -> replicate n 0.0
    , functions:
        [ benchFn "Free v5.2.0" (Trampolinef686f5f.runTrampoline <<< binds)
        , benchFn "Free v0.6.1" (Trampoline0df59c5.runTrampoline <<< bindsT)
        ]
    }
  where
  inputsPerSize :: Int
  inputsPerSize = 100

  binds :: Array Number -> Trampolinef686f5f.Trampoline Number
  binds as = foldl (\b a -> gen a >>= const b) (gen 0.0) as

  gen :: forall a. a -> Trampolinef686f5f.Trampoline a
  gen = Freef686f5f.suspendF <<< Trampolinef686f5f.done

  bindsT :: Array Number -> Trampoline0df59c5.Trampoline Number
  bindsT as = foldl (\b a -> genT a >>= const b) (genT 0.0) as

  genT :: forall a. a -> Trampoline0df59c5.Trampoline a
  genT = Trampoline0df59c5.suspend <<< Trampoline0df59c5.done

leftBindLargeBenchmark :: Benchmark
leftBindLargeBenchmark =
  mkBenchmark
    { slug: "left-bind-large"
    , title: "Left associated binds (large - " <> show inputsPerSize <> " input per size)"
    , sizes: [ 1, 5, 10, 15, 20, 25, 30 ] <#> (_ * 100_000)
    , sizeInterpretation: "Number of binds"
    , inputsPerSize: inputsPerSize
    , gen: \n -> replicate n 0.0
    , functions:
        [ benchFn "Free v5.2.0" (Trampolinef686f5f.runTrampoline <<< binds)
        -- Disabled due to stack overflow
        -- , benchFn "Free v0.6.1" (Trampoline0df59c5.runTrampoline <<< _bindsT)
        ]
    }
  where
  inputsPerSize :: Int
  inputsPerSize = 1

  binds :: Array Number -> Trampolinef686f5f.Trampoline Number
  binds as = foldl (\b a -> b >>= const (gen a)) (gen 0.0) as

  gen :: forall a. a -> Trampolinef686f5f.Trampoline a
  gen = Freef686f5f.suspendF <<< Trampolinef686f5f.done

  _bindsT :: Array Number -> Trampoline0df59c5.Trampoline Number
  _bindsT as = foldl (\b a -> b >>= const (genT a)) (genT 0.0) as

  genT :: forall a. a -> Trampoline0df59c5.Trampoline a
  genT = Trampoline0df59c5.suspend <<< Trampoline0df59c5.done

rightBindLargeBenchmark :: Benchmark
rightBindLargeBenchmark =
  mkBenchmark
    { slug: "right-bind-large"
    , title: "Right associated binds (large - " <> show inputsPerSize <> " input per size)"
    , sizes: [ 1, 5, 10, 15, 20, 25, 30 ] <#> (_ * 100_000)
    , sizeInterpretation: "Number of binds"
    , inputsPerSize: inputsPerSize
    , gen: \n -> replicate n 0.0
    , functions:
        [ benchFn "Free v5.2.0" (Trampolinef686f5f.runTrampoline <<< binds)
        , benchFn "Free v0.6.1" (Trampoline0df59c5.runTrampoline <<< bindsT)
        ]
    }
  where
  inputsPerSize :: Int
  inputsPerSize = 1

  binds :: Array Number -> Trampolinef686f5f.Trampoline Number
  binds as = foldl (\b a -> gen a >>= const b) (gen 0.0) as

  gen :: forall a. a -> Trampolinef686f5f.Trampoline a
  gen = Freef686f5f.suspendF <<< Trampolinef686f5f.done

  bindsT :: Array Number -> Trampoline0df59c5.Trampoline Number
  bindsT as = foldl (\b a -> genT a >>= const b) (genT 0.0) as

  genT :: forall a. a -> Trampoline0df59c5.Trampoline a
  genT = Trampoline0df59c5.suspend <<< Trampoline0df59c5.done
