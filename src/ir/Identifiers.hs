{-|

Types for different kinds of identifiers

-}

module Identifiers where

-- | An identifier of a variable, a method, a parameter, etc.
newtype Name = Name String deriving (Read, Eq)
instance Show Name where
  show (Name n) = n

-- | A type identifier
newtype Type = Type String deriving (Read, Eq)
instance Show Type where
  show (Type t) = t

