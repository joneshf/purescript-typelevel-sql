module SQL where

import Data.Foldable (intercalate)
import Data.List ((:))
import Data.List.Types (List)
import Data.Monoid (mempty)
import Data.Semigroup ((<>))
import Data.Semiring ((+))
import Data.Show (show)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Type.Row (class RowToList, Cons, Nil, RLProxy(..), kind RowList)

foreign import kind SQL

foreign import data SELECT :: # Type -> SQL

foreign import data FROM :: Symbol -> SQL -> SQL

foreign import data LIMIT :: Nat -> SQL -> SQL

data SQLProxy (sql :: SQL)
  = SQLProxy

type ApplyFlipped (x :: SQL) (f :: SQL -> SQL) = f x

infix 0 type ApplyFlipped as #

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
  :: ( IsSymbol table
     , ToSQL sql
     )
  => ToSQL (FROM table sql) where
    toSQL _ =
      toSQL (SQLProxy :: SQLProxy sql) <> " FROM " <> reflectSymbol (SProxy :: SProxy table)

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

foreign import kind Nat

foreign import data Z :: Nat

foreign import data S :: Nat -> Nat

data NProxy (nat :: Nat)
  = NProxy

class ToInt (nat :: Nat) where
  toInt :: NProxy nat -> Int

instance toIntZ :: ToInt Z where
  toInt _ = 0

instance toIntS :: (ToInt n) => ToInt (S n) where
  toInt _ = 1 + toInt (NProxy :: NProxy n)
