module Benchmark.Harness (Benchmark, benchFn, mkBenchmark, runSuite) where

import Prelude

import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (replicate)
import Data.Foldable (foldl)
import Data.Int (toNumber)
import Data.Number (sqrt)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (error, log)
import Performance.Minibench (benchWith')

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

-- Keep input generation outside timing and batch mapping inside it.
runSuite :: Array Benchmark -> Effect Unit
runSuite suite = do
  reports <- traverse runBenchmark suite
  log (stringify (encodeJson reports))
  where
  runBenchmark benchmark = do
    measurements <- traverse (measureSize benchmark) benchmark.sizes
    pure
      { slug: benchmark.slug
      , title: benchmark.title
      , sizeInterpretation: benchmark.sizeInterpretation
      , unit: "seconds per input batch"
      , inputsPerSize: benchmark.inputsPerSize
      , measurements
      }

  measureSize benchmark size = do
    error (benchmark.slug <> ": n=" <> show size)
    let
      inputs = map (\_ -> benchmark.gen size) (replicate benchmark.inputsPerSize unit)
      sampleCount = if size <= 5 then 1000 else if size <= 1000 then 100 else 5
      warmupCount = if size <= 1000 then 100 else 1
      iterationsPerSample = if size <= 5 then 100 else 1
      repetitions = replicate iterationsPerSample unit
    results <- traverse
      ( \fn -> do
          let
            runBatch _ =
              if iterationsPerSample == 1 then
                map fn.fn inputs
              else
                foldl (\_ _ -> map fn.fn inputs) [] repetitions
          -- Let the runtime optimize the small cases before collecting statistics.
          _ <- benchWith' warmupCount runBatch
          measured <- benchWith' sampleCount runBatch
          let
            divisor = 1.0e9 * toNumber iterationsPerSample
            mean = measured.mean / divisor
            deviation = measured.stdDev / divisor
            sem = deviation / sqrt (toNumber sampleCount)
          pure
            { name: fn.name
            , sampleCount
            , warmupCount
            , iterationsPerSample
            , mean
            , deviation
            , sem
            , relativeStandardErrorPercent: 100.0 * sem / mean
            , min: measured.min / divisor
            , max: measured.max / divisor
            }
      )
      benchmark.functions
    pure { size, results }
