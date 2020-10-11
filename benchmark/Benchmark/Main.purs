module Benchmark.Main (main) where

import Prelude

import Benchmark.Free as Free
import Benchmark.Freef686f5f as Freef686f5f
import Benchmark.Trampoline as Trampoline
import Benchmark.Trampoline0df59c5 as Trampoline0df59c5
import Benchmark.Trampolinef686f5f as Trampolinef686f5f
import Benchotron.Core (Benchmark, benchFn, mkBenchmark)
import Benchotron.UI.Console (runSuite)
import Data.Foldable (foldl)
import Effect (Effect)
import Test.QuickCheck.Gen (vectorOf)

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
    , sizes: [1, 2, 3, 4, 5, 10, 20, 30, 40, 50, 100, 250, 500, 1000]
    , sizeInterpretation: "Number of binds"
    , inputsPerSize: inputsPerSize
    , gen: \n -> vectorOf n (pure 0.0)
    , functions:
        [ benchFn "Free (master)" (Trampoline.runTrampoline <<< binds)
        , benchFn "Free v5.2.0" (Trampolinef686f5f.runTrampoline <<< binds_5_2_0)
        , benchFn "Free v0.6.1" (Trampoline0df59c5.runTrampoline <<< binds_0_6_1)
        ]
    }
  where
  inputsPerSize :: Int
  inputsPerSize = 100

  binds :: Array Number -> Trampoline.Trampoline Number
  binds as = foldl (\b a -> b >>= const (gen a)) (gen 0.0) as

  gen :: forall a. a -> Trampoline.Trampoline a
  gen = Free.suspend <<< Trampoline.done

  binds_5_2_0 :: Array Number -> Trampolinef686f5f.Trampoline Number
  binds_5_2_0 as = foldl (\b a -> b >>= const (gen_5_2_0 a)) (gen_5_2_0 0.0) as

  gen_5_2_0 :: forall a. a -> Trampolinef686f5f.Trampoline a
  gen_5_2_0 = Freef686f5f.suspendF <<< Trampolinef686f5f.done

  binds_0_6_1 :: Array Number -> Trampoline0df59c5.Trampoline Number
  binds_0_6_1 as = foldl (\b a -> b >>= const (gen_0_6_1 a)) (gen_0_6_1 0.0) as

  gen_0_6_1 :: forall a. a -> Trampoline0df59c5.Trampoline a
  gen_0_6_1 = Trampoline0df59c5.suspend <<< Trampoline0df59c5.done

rightBindSmallBenchmark :: Benchmark
rightBindSmallBenchmark =
  mkBenchmark
  { slug: "right-bind-small"
  , title: "Right associated binds (small - " <> show inputsPerSize <> " inputs per size)"
  , sizes: [1, 2, 3, 4, 5, 10, 20, 30, 40, 50, 100, 250, 500, 1000]
  , sizeInterpretation: "Number of binds"
  , inputsPerSize: inputsPerSize
  , gen: \n -> vectorOf n (pure 0.0)
  , functions:
      [ benchFn "Free (master)" (Trampoline.runTrampoline <<< binds)
      , benchFn "Free v5.2.0" (Trampolinef686f5f.runTrampoline <<< binds_5_2_0)
      , benchFn "Free v0.6.1" (Trampoline0df59c5.runTrampoline <<< binds_0_6_1)
      ]
  }
  where
  inputsPerSize :: Int
  inputsPerSize = 100

  binds :: Array Number -> Trampoline.Trampoline Number
  binds as = foldl (\b a -> gen a >>= const b) (gen 0.0) as

  gen :: forall a. a -> Trampoline.Trampoline a
  gen = Free.suspend <<< Trampoline.done

  binds_5_2_0 :: Array Number -> Trampolinef686f5f.Trampoline Number
  binds_5_2_0 as = foldl (\b a -> gen_5_2_0 a >>= const b) (gen_5_2_0 0.0) as

  gen_5_2_0 :: forall a. a -> Trampolinef686f5f.Trampoline a
  gen_5_2_0 = Freef686f5f.suspendF <<< Trampolinef686f5f.done

  binds_0_6_1 :: Array Number -> Trampoline0df59c5.Trampoline Number
  binds_0_6_1 as = foldl (\b a -> gen_0_6_1 a >>= const b) (gen_0_6_1 0.0) as

  gen_0_6_1 :: forall a. a -> Trampoline0df59c5.Trampoline a
  gen_0_6_1 = Trampoline0df59c5.suspend <<< Trampoline0df59c5.done

leftBindLargeBenchmark :: Benchmark
leftBindLargeBenchmark =
  mkBenchmark
    { slug: "left-bind-large"
    , title: "Left associated binds (large - " <> show inputsPerSize <> " input per size)"
    , sizes: [1, 5, 10, 15, 20, 25, 30 ] <#> (_ * 100_000)
    , sizeInterpretation: "Number of binds"
    , inputsPerSize: inputsPerSize
    , gen: \n -> vectorOf n (pure 0.0)
    , functions:
        [ benchFn "Free (master)" (Trampoline.runTrampoline <<< binds)
        , benchFn "Free v5.2.0" (Trampolinef686f5f.runTrampoline <<< binds_5_2_0)
        -- Disabled due to stack overflow
        -- , benchFn "Free v0.6.1" (Trampoline0df59c5.runTrampoline <<< binds_0_6_1)
        ]
    }
  where
  inputsPerSize :: Int
  inputsPerSize = 1

  binds :: Array Number -> Trampoline.Trampoline Number
  binds as = foldl (\b a -> b >>= const (gen a)) (gen 0.0) as

  gen :: forall a. a -> Trampoline.Trampoline a
  gen = Free.suspend <<< Trampoline.done

  binds_5_2_0 :: Array Number -> Trampolinef686f5f.Trampoline Number
  binds_5_2_0 as = foldl (\b a -> b >>= const (gen_5_2_0 a)) (gen_5_2_0 0.0) as

  gen_5_2_0 :: forall a. a -> Trampolinef686f5f.Trampoline a
  gen_5_2_0 = Freef686f5f.suspendF <<< Trampolinef686f5f.done

  binds_0_6_1 :: Array Number -> Trampoline0df59c5.Trampoline Number
  binds_0_6_1 as = foldl (\b a -> b >>= const (gen_0_6_1 a)) (gen_0_6_1 0.0) as

  gen_0_6_1 :: forall a. a -> Trampoline0df59c5.Trampoline a
  gen_0_6_1 = Trampoline0df59c5.suspend <<< Trampoline0df59c5.done

rightBindLargeBenchmark :: Benchmark
rightBindLargeBenchmark =
  mkBenchmark
    { slug: "right-bind-large"
    , title: "Right associated binds (large - " <> show inputsPerSize <> " input per size)"
    , sizes: [1, 5, 10, 15, 20, 25, 30 ] <#> (_ * 100_000)
    , sizeInterpretation: "Number of binds"
    , inputsPerSize: inputsPerSize
    , gen: \n -> vectorOf n (pure 0.0)
    , functions:
        [ benchFn "Free (master)" (Trampoline.runTrampoline <<< binds)
        , benchFn "Free v5.2.0" (Trampolinef686f5f.runTrampoline <<< binds_5_2_0)
        , benchFn "Free v0.6.1" (Trampoline0df59c5.runTrampoline <<< binds_0_6_1)
        ]
    }
  where
  inputsPerSize :: Int
  inputsPerSize = 1

  binds :: Array Number -> Trampoline.Trampoline Number
  binds as = foldl (\b a -> gen a >>= const b) (gen 0.0) as

  gen :: forall a. a -> Trampoline.Trampoline a
  gen = Free.suspend <<< Trampoline.done

  binds_5_2_0 :: Array Number -> Trampolinef686f5f.Trampoline Number
  binds_5_2_0 as = foldl (\b a -> gen_5_2_0 a >>= const b) (gen_5_2_0 0.0) as

  gen_5_2_0 :: forall a. a -> Trampolinef686f5f.Trampoline a
  gen_5_2_0 = Freef686f5f.suspendF <<< Trampolinef686f5f.done

  binds_0_6_1 :: Array Number -> Trampoline0df59c5.Trampoline Number
  binds_0_6_1 as = foldl (\b a -> gen_0_6_1 a >>= const b) (gen_0_6_1 0.0) as

  gen_0_6_1 :: forall a. a -> Trampoline0df59c5.Trampoline a
  gen_0_6_1 = Trampoline0df59c5.suspend <<< Trampoline0df59c5.done
