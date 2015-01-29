{-|

Types for different kinds of identifiers

-}

module Identifiers where

-- | An identifier of a variable, a method, a parameter, etc.
newtype Name = Name String deriving (Read, Eq)
instance Show Name where
  show (Name n) = n

thisName :: Name
thisName = Name "this"

-- | The supported (infix) operators
data Op = AND
        | OR
        | NOT
        | LT
        | GT
        | LTE
        | GTE
        | EQ
        | NEQ
        | PLUS
        | MINUS
        | TIMES
        | DIV
        | MOD
        deriving(Read, Eq)

instance Show Op where
    show AND            = "and"
    show OR             = "or"
    show NOT            = "not"
    show Identifiers.LT = "<"
    show Identifiers.GT = ">"
    show Identifiers.EQ = "="
    show NEQ            = "!="
    show PLUS           = "+"
    show MINUS          = "-"
    show TIMES          = "*"
    show DIV            = "/"
    show MOD            = "%"
