module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import SQL.Table (Table)
import Type.Nat (TWO)
import Type.SQL (FROM, JOIN, LIMIT, SELECT, SQLProxy(..), toSQL)
import Type.SQL.Function (type (#))

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log $ toSQL (SQLProxy :: SQLProxy Query)

type Query
  = SELECT ("quux.foo" :: Int, bar :: String, "corge.foo" :: Boolean)
  # FROM Quux
  # JOIN Corge
  # LIMIT TWO

type Quux
  = Table
    "quux"
    ( foo :: Int
    , bar :: String
    , baz :: Boolean
    )

type Corge
  = Table
    "corge"
    ( foo :: Boolean
    )
