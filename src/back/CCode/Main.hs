{-# LANGUAGE GADTs,FlexibleInstances,FlexibleContexts,MultiParamTypeClasses,StandaloneDeriving #-}

{-| Provides the CCode data type, a representation of C
programs that can be pretty-printed to sometimes-legal C code. The
purpose of this data type is NOT to guarantee that only valid C code
will be generated, but it tries to enforce some reasonable invariants.
-}

module CCode.Main where

import qualified AST
import Data.Char

data Toplevel
data Stat
data Expr
data Fun
data Incl
data Name
data Ty
data Lval
-- | CCode FIN == A complete program
data FIN

-- the next lines are magic :)

-- | The UsableAs typeclass marks what might go where in the
-- | CCode type below. For instance, you can always use an lval where
-- | an expression is expected, and you can always use a Name where an
-- | Lval is expected
class UsableAs a b where

instance UsableAs Name Lval where
instance UsableAs Lval Expr where
instance UsableAs Name Expr where
instance UsableAs a a where

type CType = String
type CName = String

type CVarSpec = (CCode Ty, CCode Lval)

data CCode a where
    Program    :: CCode Toplevel -> CCode FIN
    Includes   :: [String] -> CCode Toplevel
    HashDefine :: String -> CCode Toplevel
    Statement  :: UsableAs e Expr => CCode e -> CCode Stat
    Switch     :: (UsableAs e Expr) => CCode e -> [(CCode Name, CCode Stat)] -> CCode Stat -> CCode Stat
    StructDecl :: CCode Ty -> [CVarSpec] -> CCode Toplevel
    Record     :: UsableAs e Expr => [CCode e] -> CCode Expr
    Assign     :: UsableAs l Lval => CCode l -> CCode Expr -> CCode Expr
    Decl       :: CVarSpec -> CCode Lval
    Concat     :: [CCode Stat] -> CCode Stat
    ConcatTL   :: [CCode Toplevel] -> CCode Toplevel -- I do not like
                                                     -- this
                                                     -- duplication. Not
                                                     -- one bit!
    StoopidSeq :: [CCode Expr] -> CCode Expr -- A.Seq is a kind of
                                             -- Expr that doesn't
                                             -- directly map to
                                             -- C-Exprs.
    Enum       :: [CCode Name] -> CCode Toplevel
    Braced     :: CCode a -> CCode a -- get rid of this; only used in
                                     -- Let-expr
    BinOp      :: CCode Name -> CCode Expr -> CCode Expr -> CCode Expr
    Dot        :: (UsableAs e Expr) => CCode e -> CCode Name -> CCode Lval
    Deref      :: UsableAs e Expr => CCode e -> CCode Expr
    Amp        :: (UsableAs e Expr) => CCode e -> CCode Expr
    Ptr        :: CCode Ty -> CCode Ty
    Function   :: CCode Ty -> CCode Name -> [CVarSpec] -> CCode Stat -> CCode Toplevel
    AsExpr     :: CCode Lval -> CCode Expr
    AsLval     :: CCode Name -> CCode Lval
    Nam        :: String -> CCode Name
    Var        :: String -> CCode Lval -- fixme this should be -> Code Lval
    Typ        :: String -> CCode Ty
    Static     :: CCode Ty -> CCode Ty
    Embed      :: String -> CCode a
    EmbedC     :: CCode a -> CCode b
    Call       :: (UsableAs e1 Expr, UsableAs e2 Expr) => CCode e1 -> [CCode e2] -> CCode Expr
    TypeDef    :: CCode Ty -> CCode Toplevel -> CCode Toplevel
