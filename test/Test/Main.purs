module Test.Main where

import Prelude

import Control.Monad.Eff.Console (log)

import Test.Benchmark

main = do
  log "Teletype"
  --Example.Teletype.main

  log ""

  log "TeletypeCoproduct"
  --Example.TeletypeCoproduct.main

  log ""

  log "TestBind"
  --Example.TestBind.main

  log ""

  log "ReinterpreterWithBindF"
  --Example.ReinterpretWithBindF.main

  log "Running benchmarks..."
  runBenchmarks
