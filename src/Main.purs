module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import SQL (type (#), FROM(..), SELECT(SELECT), toSQL)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  let query :: SELECT (foo :: Int, bar :: String, baz :: Boolean) # FROM "quux"
      query = SELECT # FROM

  log $ toSQL query
