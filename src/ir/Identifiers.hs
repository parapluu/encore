{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

{-|

Types for different kinds of identifiers

-}

module Identifiers where

import Data.List(intercalate)
import Data.Maybe(isNothing)

-- | An identifier of a variable, a method, a parameter, etc.
newtype Name = Name String deriving (Read, Eq, Ord)

instance Show Name where
  show (Name n) = n

-- | Type of qualified names
data Path = Path [Name] deriving (Eq, Ord)

emptyPath = Path []

extendPath :: Path -> Name -> Path
extendPath (Path path) name = Path (path ++ [name])

name2path :: Name -> Path
name2path n = Path [n]

string2path :: String -> Path
string2path s = Path [Name s]

decompose :: Path -> (Maybe Path, Name)
decompose (Path []) = error "Identifiers.hs: tried to decompose empty path"
decompose (Path [n]) = (Nothing, n)
decompose (Path l) = (Just $ Path $ init l, last l)

qualify :: Path -> Path -> Path
qualify (Path namespace) path = let (_, base) = decompose path in Path (namespace ++ [base])

unsafeBase :: Int -> Path -> Name
unsafeBase n path = base path

base :: Path -> Name
base = snd . decompose

ns :: Path -> Maybe Path
ns = fst . decompose

getNamespace :: Path -> Path
getNamespace (Path [])  = error "Identifiers.hs: tried to get namespace of empty path"
getNamespace (Path [_]) = Path []
getNamespace (Path l)   = Path $ init l

qualified :: Path -> Bool
qualified = not . isNothing . fst . decompose

instance Show Path where
  show (Path []) = "<default>"
  show (Path qn) = intercalate "." (map (\(Name n) -> n) qn)

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
