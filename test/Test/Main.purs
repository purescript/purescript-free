module Test.Main where

import Prelude

import Control.Monad.Eff.Console (log)

import qualified Test.Control.Monad.Free.Coproduct as C
import qualified Test.Control.Monad.Free.Stratified as S
import qualified Test.Control.Monad.Free.Teletype as T

main = do
  log "Teletype"
  T.main

  log ""

  log "Coproduct"
  C.main

  log ""

  log "Stratified"
  S.main
