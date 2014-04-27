{-# LANGUAGE GADTs,FlexibleInstances,FlexibleContexts,MultiParamTypeClasses #-}

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

data Toplevel
data Stat
data Expr
data Fun
data Incl
data Id
data Ty
data Lval
data FIN

-- the next lines are magic
class UsableAs a b where
instance UsableAs Id Lval where
instance UsableAs Lval Expr where
instance UsableAs Id Expr where
instance UsableAs a a where
--instance (UsableAs a b, UsableAs b c) => UsableAs a c where

type CType = String
type CId = String

type CVarSpec = (CCode Ty, CCode Id)

data CCode a where
    Program    :: CCode Toplevel -> CCode FIN
    Includes   :: [String] -> CCode Toplevel
    HashDefine :: String -> CCode Toplevel
    Statement  :: UsableAs e Expr => CCode e -> CCode Stat
    Switch     :: CCode Id -> [(CCode Id, CCode Stat)] -> CCode Stat -> CCode Stat
    StructDecl :: CCode Ty -> [CVarSpec] -> CCode Toplevel
    Record     :: UsableAs e Expr => [CCode e] -> CCode Expr
    Assign     :: UsableAs l Lval => CCode l -> CCode Expr -> CCode Expr
    Decl       :: CVarSpec -> CCode Lval
    Concat     :: [CCode Stat] -> CCode Stat
    ConcatTL   :: [CCode Toplevel] -> CCode Toplevel
    StoopidSeq :: [CCode Expr] -> CCode Expr -- A.Seq is a kind of Expr that doesn't directly map to C-Exprs
    Enum       :: [CCode Id] -> CCode Toplevel
    Braced     :: CCode a -> CCode a -- get rid of this; only used in Let-expr
    BinOp      :: CCode Id -> CCode Expr -> CCode Expr -> CCode Expr
    Dot        :: CCode Expr -> CCode Id -> CCode Lval
    Deref      :: CCode Expr -> CCode Expr
    Ptr        :: CCode Ty -> CCode Ty
    Function   :: CCode Ty -> CCode Id -> [CVarSpec] -> CCode Stat -> CCode Toplevel
    AsExpr     :: CCode Lval -> CCode Expr
    AsLval     :: CCode Id -> CCode Lval
    Var        :: String -> CCode Id -- fixme this should be -> Code Lval
    Typ        :: String -> CCode Ty
    Embed      :: String -> CCode a
    EmbedC     :: CCode a -> CCode b
    Call       :: (UsableAs e1 Expr, UsableAs e2 Expr) => CCode e1 -> [CCode e2] -> CCode Expr
    TypeDef    :: CCode Ty -> CCode Toplevel -> CCode Toplevel
