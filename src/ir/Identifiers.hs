{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

{-|

Types for different kinds of identifiers

-}

module Identifiers where

import Data.List
import Data.Maybe
import Text.Parsec.Pos as P

-- | An identifier of a variable, a method, a parameter, etc.
newtype Name = Name String deriving (Read, Eq, Ord)
instance Show Name where
  show (Name n) = n

type Namespace = [Name]

showNamespace :: Namespace -> String
showNamespace = intercalate "." . map show

data QualifiedName =
    QName{qnspace  :: Maybe Namespace
         ,qnsource :: Maybe SourceName
         ,qnlocal  :: Name
         } deriving(Eq)

instance Show QualifiedName where
    show QName{qnspace = Just [], qnlocal} =
        show qnlocal
    show QName{qnspace = Just ns, qnlocal} =
        showNamespace ns ++ "." ++ show qnlocal
    show QName{qnspace = Nothing, qnlocal} =
        show qnlocal

topLevelQName = QName (Just []) Nothing
qLocal = QName Nothing Nothing
qName = qLocal . Name

setNamespace :: Namespace -> QualifiedName -> QualifiedName
setNamespace ns qname = qname{qnspace = Just ns}

setSourceFile :: SourceName -> QualifiedName -> QualifiedName
setSourceFile source qname = qname{qnsource = Just source}

isLocalQName :: QualifiedName -> Bool
isLocalQName QName{qnspace} = isNothing qnspace

thisName :: Name
thisName = Name "this"

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
