module Type.Nat where

import Data.Semiring ((+))

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

type ZERO = Z
type ONE = S ZERO
type TWO = S ONE
