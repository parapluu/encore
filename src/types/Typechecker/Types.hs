{-|

The types used by the "Typechecker".

-}

module Typechecker.Types(VarType, FieldType, MethodType, ClassType) where

import AST.AST
import Identifiers

type VarType = (Name, Type)
type FieldType = (Name, Type)
type MethodType = (Name, (Type, [ParamDecl]))
type ClassType = (Type, ([FieldType], [MethodType]))