{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

{-|

Types for different kinds of identifiers

-}

module Identifiers where

-- | An identifier of a variable, a method, a parameter, etc.
newtype Name = Name String deriving (Read, Eq, Ord)
instance Show Name where
  show (Name n) = n

-- ! Type of qualified names
type QName = [Name]

thisName :: Name
thisName = Name "this"

constructorName :: Name
constructorName = Name "_init"

-- | The supported binary operators
data BinaryOp = AND
              | OR
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

instance Show BinaryOp where
    show Identifiers.AND = "and"
    show Identifiers.OR  = "or"
    show Identifiers.LT  = "<"
    show Identifiers.GT  = ">"
    show Identifiers.LTE = "<="
    show Identifiers.GTE = ">="
    show Identifiers.EQ  = "="
    show NEQ             = "!="
    show PLUS            = "+"
    show MINUS           = "-"
    show TIMES           = "*"
    show DIV             = "/"
    show MOD             = "%"

data UnaryOp = NOT
             | NEG
               deriving(Read, Eq)

instance Show UnaryOp where
    show Identifiers.NOT = "not"
    show Identifiers.NEG = "-"
