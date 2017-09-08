module Type.SQL.Function where

import Type.SQL (kind SQL)

type ApplyFlipped (x :: SQL) (f :: SQL -> SQL) = f x

infix 0 type ApplyFlipped as #
