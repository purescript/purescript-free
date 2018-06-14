module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Test.Control.Monad.Free.Coproduct as C
import Test.Control.Monad.Free.Stratified as S
import Test.Control.Monad.Free.Teletype as T

main :: Effect Unit
main = do
  log "Teletype"
  T.main

  log ""

  log "Coproduct"
  C.main

  log ""

  log "Stratified"
  S.main
