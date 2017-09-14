module SQL.Table where


foreign import kind COLUMN

foreign import kind TABLE

foreign import data Integer :: COLUMN

foreign import data Table :: Symbol -> # COLUMN -> TABLE

data TProxy (table :: TABLE)
  = TProxy
