module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import SQL (type (#), FROM(..), LIMIT(..), S, SELECT(..), Z, toSQL)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  let query :: SELECT (foo :: Int, bar :: String) # FROM "quux" # LIMIT (S (S Z))
      query = SELECT # FROM # LIMIT

  log $ toSQL query
