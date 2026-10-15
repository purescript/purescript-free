module Benchmark.Harness (Benchmark, benchFn, mkBenchmark, runSuite) where

import Prelude

import Effect (Effect)

type Benchmark =
  { slug :: String
  , title :: String
  , sizes :: Array Int
  , sizeInterpretation :: String
  , inputsPerSize :: Int
  , gen :: Int -> Array Number
  , functions :: Array { name :: String, fn :: Array Number -> Number }
  }

mkBenchmark :: Benchmark -> Benchmark
mkBenchmark = identity

benchFn :: String -> (Array Number -> Number) -> { name :: String, fn :: Array Number -> Number }
benchFn name fn = { name, fn }

-- Keep PureScript's batch map inside the timed operation, as in Benchotron.
runSuite :: Array Benchmark -> Effect Unit
runSuite = runSuiteImpl map

foreign import runSuiteImpl
  :: ((Array Number -> Number) -> Array (Array Number) -> Array Number)
  -> Array Benchmark
  -> Effect Unit
