module SQL.Table where

import Type.Row (class RowToList, kind RowList)

foreign import kind TABLE

foreign import data Table :: Symbol -> # Type -> TABLE

data TProxy (table :: TABLE)
  = TProxy

class TableColumns (table :: TABLE) (columns :: RowList) | table -> columns

instance tableColumnsTable
  :: ( RowToList cs columns
     )
  => TableColumns (Table name cs) columns

class TableName (table :: TABLE) (name :: Symbol) | table -> name

instance toSQLTableNameTable :: TableName (Table name columns) name
