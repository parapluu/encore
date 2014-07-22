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
data Op = LT | GT | EQ | NEQ | PLUS | MINUS | TIMES | DIV | MOD deriving(Read, Eq)
instance Show Op where
    show Identifiers.LT = "<"
    show Identifiers.GT = ">"
    show Identifiers.EQ = "="
    show NEQ            = "!="
    show PLUS           = "+"
    show MINUS          = "-"
    show TIMES          = "*"
    show DIV            = "/"
    show MOD            = "%"
