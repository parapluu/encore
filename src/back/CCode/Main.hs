{-# LANGUAGE GADTs,FlexibleInstances #-}

module CCode.Main 
--    (CCode (..),
--     CVarSpec (..),
--     Id (..),
--     embedCType,
--     CType,
--     show)
        where

import qualified AST
import Data.Char

embedCType :: String -> CCode Ty
embedCType = Typ

data Toplevel
data Stat
data Expr
data Fun
data Incl
data Id
data Ty
data Lval
data FIN

type CType = String
type CId = String

type CVarSpec = (CCode Ty, CCode Id)

data CCode a where
    Program     :: CCode Toplevel -> CCode FIN
    Includes    :: [String] -> CCode Toplevel
    HashDefine  :: String -> CCode Toplevel
    Statement   :: CCode Expr -> CCode Stat
    Switch      :: CCode Id -> [(CCode Id, CCode Stat)] -> CCode Stat -> CCode Stat
    StructDecl  :: CCode Ty -> [CVarSpec] -> CCode Toplevel
    Record      :: [CCode Expr] -> CCode Expr
    Assign      :: CCode Lval -> CCode Expr -> CCode Expr
    Decl        :: CVarSpec -> CCode Lval
    Concat      :: [CCode Stat] -> CCode Stat
    ConcatTL    :: [CCode Toplevel] -> CCode Toplevel
    StoopidSeq  :: [CCode Expr] -> CCode Expr -- A.Seq is a kind of Expr that doesn't directly map to C-Exprs
    Enum        :: [CCode Id] -> CCode Toplevel
    Braced      :: CCode a -> CCode a -- get rid of this; only used in Let-expr
    BinOp       :: CCode Id -> CCode Expr -> CCode Expr -> CCode Expr
    Dot         :: CCode Expr -> CCode Id -> CCode Lval
    Deref       :: CCode Expr -> CCode Expr
    Ptr         :: CCode Ty -> CCode Ty
    Function    :: CCode Ty -> CCode Id -> [CVarSpec] -> CCode Stat -> CCode Toplevel
    AsExpr :: CCode Lval -> CCode Expr
    AsLval :: CCode Id -> CCode Lval
    Var :: String -> CCode Id -- fixme this should be -> Code Lval
    Typ :: String -> CCode Ty
    Embed :: String -> CCode a
    EmbedC :: CCode a -> CCode b
    Call :: CCode Expr -> [CCode Expr] -> CCode Expr
    TypeDef :: CCode Ty -> CCode Toplevel -> CCode Toplevel

--to_expr :: CCode Lval -> CCode 
--to_expr (Decl lval) = 


--newtype CType = CType String
--type Id = String
--
--instance Show CType where
--  show (CType ct) = ct
--
--embedCType :: String -> CType
--embedCType = CType
--
--newtype CVarSpec = CVarSpec (CType, Id)
--
--data CCode =
--     Includes [String]
--   | Decl CVarSpec
--   | HashDefine String
--   | Switch Id [(CCode, CCode)] CCode
--   | StructDecl Id [CVarSpec]
--   | Record [CCode]
--   | Assign CCode CCode
--   | Statement CCode -- for putting a semi-colon on the end.
--   | C [CCode]
--   | Enum [Id]
--   | BracedBlock CCode
--   | Call Id [CCode]
--   | TypeDef Id CCode
--   | Deref CCode
--   | Dot CCode Id
--   | Var Id
--   | Embed String  -- for C code that doesn't match other patterns
--   | Function { fun_ret :: CType,
--                fun_name :: String,
--                fun_args :: [CVarSpec],
--                fun_body :: [CCode] }
--   | FwdDecl CCode
