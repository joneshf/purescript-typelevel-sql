module SQL where

import Data.Foldable (intercalate)
import Data.List ((:))
import Data.List.Types (List)
import Data.Monoid (mempty)
import Data.Semigroup ((<>))
import Data.Semiring ((+))
import Data.Show (show)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Type.Proxy (Proxy(..))
import Type.Row (class RowToList, Cons, Nil, RLProxy(..), kind RowList)

data SELECT (columns :: # Type)

data FROM (table :: Symbol) a

data LIMIT (count :: Nat) a

type ApplyFlipped x f = f x

infix 0 type ApplyFlipped as #

class ToSQL a where
  toSQL :: Proxy a -> String

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
      toSQL (Proxy :: Proxy sql) <> " FROM " <> reflectSymbol (SProxy :: SProxy table)

instance toSQLLIMIT
  :: ( ToInt count
     , ToSQL sql
     )
  => ToSQL (LIMIT count sql) where
    toSQL _ =
      toSQL (Proxy :: Proxy sql) <> " LIMIT " <> show (toInt (NProxy :: NProxy count))

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
