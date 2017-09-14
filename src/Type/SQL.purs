module Type.SQL where

import Data.Foldable (intercalate)
import Data.List ((:))
import Data.List.Types (List)
import Data.Monoid (mempty)
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import SQL.Table (class TableColumns, class TableName, kind TABLE)
import Type.Data.Symbol (class AppendSymbol)
import Type.Nat (class ToInt, NProxy(..), toInt, kind Nat)
import Type.Row (class ListToRow, class RowToList, Cons, Nil, RLProxy(..), kind RowList)

foreign import kind SQL

foreign import data SELECT :: # Type -> SQL

foreign import data FROM :: TABLE -> SQL -> SQL

foreign import data JOIN :: TABLE -> SQL -> SQL

foreign import data LIMIT :: Nat -> SQL -> SQL

data SQLProxy (sql :: SQL)
  = SQLProxy

class Prefixed (prefix :: Symbol) (rowList :: RowList) (prefixed :: RowList)
  | prefix rowList -> prefixed

instance prefixedNil :: Prefixed prefix Nil Nil

instance prefixedCons
  :: ( AppendSymbol prefix label prefixedLabel
     , Prefixed prefix rowTail prefixedTail
     )
  => Prefixed prefix (Cons label ty rowTail) (Cons prefixedLabel ty prefixedTail)

class SQLColumns (sql :: SQL) (columns :: RowList) | sql -> columns

instance sqlColumnsSELECT
  :: ( RowToList cs columns
     )
  => SQLColumns (SELECT cs) columns

instance sqlColumnsFROM
  :: ( SQLColumns sql columns
     )
  => SQLColumns (FROM table sql) columns

class TableRow (sql :: SQL) (row :: # Type) | sql -> row

instance tableRowFROM
  :: ( AppendSymbol tableName "." dottedTableName
     , IsSymbol tableName
     , ListToRow prefixedTableColumns prefixedTableRow
     , ListToRow sqlColumns sqlRow
     , ListToRow tableColumns tableRow
     , Prefixed dottedTableName tableColumns prefixedTableColumns
     , SQLColumns sql sqlColumns
     , TableColumns table tableColumns
     , TableName table tableName
     , Union prefixedTableRow tableRow allTableRows
     , Union sqlRow who_cares allTableRows
     )
  => TableRow (FROM table sql) allTableRows

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
  :: ( AppendSymbol tableName "." dottedTableName
     , IsSymbol tableName
     , ListToRow prefixedTableColumns prefixedTableRow
     , ListToRow sqlColumns sqlRow
     , ListToRow tableColumns tableRow
     , Prefixed dottedTableName tableColumns prefixedTableColumns
     , SQLColumns sql sqlColumns
     , TableColumns table tableColumns
     , TableName table tableName
     , ToSQL sql
     , Union prefixedTableRow tableRow allTableRows
     , Union sqlRow who_cares allTableRows
     )
  => ToSQL (FROM table sql) where
    toSQL _ =
      toSQL (SQLProxy :: SQLProxy sql) <> " FROM " <> reflectSymbol (SProxy :: SProxy tableName)

instance toSQLJOIN
  :: ( AppendSymbol fromTableName "." fromDottedTableName
     , AppendSymbol joinTableName "." joinDottedTableName
     , IsSymbol fromTableName
     , IsSymbol joinTableName
     , ListToRow fromPrefixedTableColumns fromPrefixedTableRow
     , ListToRow fromTableColumns fromTableRow
     , ListToRow joinPrefixedTableColumns joinPrefixedTableRow
     , ListToRow joinTableColumns joinTableRow
     , ListToRow sqlColumns sqlRow
     , Prefixed fromDottedTableName fromTableColumns fromPrefixedTableColumns
     , Prefixed joinDottedTableName joinTableColumns joinPrefixedTableColumns
     , SQLColumns sql sqlColumns
     , TableColumns fromTable fromTableColumns
     , TableColumns joinTable joinTableColumns
     , TableName fromTable fromTableName
     , TableName joinTable joinTableName
     , ToSQL sql
     , Union fromAllTableRows joinAllTableRows allTableRows
     , Union fromPrefixedTableRow fromTableRow fromAllTableRows
     , Union joinPrefixedTableRow joinTableRow joinAllTableRows
     , Union sqlRow who_cares allTableRows
     )
  => ToSQL (JOIN joinTable (FROM fromTable sql)) where
    toSQL _ =
      toSQL (SQLProxy :: SQLProxy sql)
        <> " FROM "
        <> reflectSymbol (SProxy :: SProxy fromTableName)
        <> " JOIN "
        <> reflectSymbol (SProxy :: SProxy joinTableName)

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
