module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import SQL (type (#), FROM, LIMIT, S, SELECT, Z, toSQL)
import Type.Proxy (Proxy(..))

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log $ toSQL (Proxy :: Proxy Query)

type Query
  = SELECT (foo :: Int, bar :: String)
  # FROM "quux"
  # LIMIT (S (S Z))
