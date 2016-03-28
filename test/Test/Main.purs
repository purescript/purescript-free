module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Test.Control.Monad.Free.Coproduct as C
import Test.Control.Monad.Free.Stratified as S
import Test.Control.Monad.Free.Teletype as T

main :: Eff (console :: CONSOLE) Unit
main = do
  log "Teletype"
  T.main

  log ""

  log "Coproduct"
  C.main

  log ""

  log "Stratified"
  S.main
