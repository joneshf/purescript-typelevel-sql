module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Type.Nat (TWO)
import Type.SQL (FROM, LIMIT, SELECT, SQLProxy(..), toSQL)
import Type.SQL.Function (type (#))

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log $ toSQL (SQLProxy :: SQLProxy Query)

type Query
  = SELECT (foo :: Int, bar :: String)
  # FROM "quux"
  # LIMIT TWO
