module SQL where

import Data.Foldable (intercalate)
import Data.List ((:))
import Data.List.Types (List)
import Data.Monoid (mempty)
import Data.Semigroup ((<>))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Type.Row (class RowToList, Cons, Nil, RLProxy(..), kind RowList)

data SELECT (columns :: # Type)
  = SELECT

data FROM (table :: Symbol) a
  = FROM a

type ApplyFlipped x f = f x

infix 0 type ApplyFlipped as #

class ToSQL a where
  toSQL :: a -> String

instance toSQLSELECT
  :: ( RowToList columns rl
     , ToSQLSELECT rl
     )
  => ToSQL (SELECT columns) where
    toSQL _ =
      "SELECT " <> intercalate ", " (toColumn (RLProxy :: RLProxy rl))

instance toSQLFROM
  :: ( IsSymbol table
     , ToSQL sql
     )
  => ToSQL (FROM table sql) where
    toSQL (FROM sql) =
      toSQL sql <> " FROM " <> reflectSymbol (SProxy :: SProxy table)

class ToSQLSELECT (columns :: RowList) where
  toColumn :: RLProxy columns -> List String

instance toSQLSELECTNil :: ToSQLSELECT Nil where
  toColumn _ = mempty

instance toSQLSELECTCons
  :: ( IsSymbol column
     , ToSQLSELECT rest
     )
  => ToSQLSELECT (Cons column don't_care rest) where
    toColumn _ =
      reflectSymbol (SProxy :: SProxy column) : toColumn (RLProxy :: RLProxy rest)
