module Type.SQL where

import Data.Foldable (intercalate)
import Data.List ((:))
import Data.List.Types (List)
import Data.Monoid (mempty)
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import SQL.Table (class TableColumns, TProxy(..), Table, kind TABLE)
import Type.Nat (class ToInt, NProxy(..), toInt, kind Nat)
import Type.Row (class ListToRow, class RowToList, Cons, Nil, RLProxy(..), kind RowList)

foreign import kind SQL

foreign import data SELECT :: # Type -> SQL

foreign import data FROM :: TABLE -> SQL -> SQL

foreign import data LIMIT :: Nat -> SQL -> SQL

data SQLProxy (sql :: SQL)
  = SQLProxy

class SQLColumns (sql :: SQL) (columns :: RowList) | sql -> columns

instance sqlColumnsSELECT
  :: ( RowToList cs columns
     )
  => SQLColumns (SELECT cs) columns

class ToSQL (sql :: SQL) where
  toSQL :: SQLProxy sql -> String

instance toSQLSELECT
  :: ( RowToList columns rl
     , ToSQLSELECT rl
     )
  => ToSQL (SELECT columns) where
    toSQL _ =
      "SELECT " <> intercalate ", " (toColumn (RLProxy :: RLProxy rl))

instance toSQLFROM
  :: ( ListToRow sqlColumns sqlRow
     , ListToRow tableColumns tableRow
     , SQLColumns sql sqlColumns
     , TableColumns table tableColumns
     , ToSQLTableName table
     , ToSQL sql
     , Union sqlRow who_cares tableRow
     )
  => ToSQL (FROM table sql) where
    toSQL _ =
      toSQL (SQLProxy :: SQLProxy sql) <> " FROM " <> toTableName (TProxy :: TProxy table)

instance toSQLLIMIT
  :: ( ToInt count
     , ToSQL sql
     )
  => ToSQL (LIMIT count sql) where
    toSQL _ =
      toSQL (SQLProxy :: SQLProxy sql) <> " LIMIT " <> show (toInt (NProxy :: NProxy count))

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

class ToSQLTableName (table :: TABLE) where
  toTableName :: TProxy table -> String

instance toSQLTableNameTable
  :: ( IsSymbol name
     )
  => ToSQLTableName (Table name columns) where
    toTableName _ = reflectSymbol (SProxy :: SProxy name)
