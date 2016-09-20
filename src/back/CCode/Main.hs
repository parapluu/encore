{-# LANGUAGE FlexibleInstances,MultiParamTypeClasses,StandaloneDeriving #-}

{-| Provides the CCode data type, a representation of C
programs that can be pretty-printed to sometimes-legal C code. The
purpose of this data type is NOT to guarantee that only valid C code
will be generated, but it tries to enforce some reasonable invariants.
-}

module CCode.Main where

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

instance UsableAs Stat Expr where

type CType = String
type CName = String

type CVarSpec = (CCode Ty, CCode Lval)

data CCode a where
    Program      :: CCode Toplevel -> CCode FIN
    Skip         :: CCode Stat
    Null         :: CCode Expr
    Includes     :: [String] -> CCode Toplevel
    LocalInclude :: String   -> CCode Toplevel
    IfDefine     :: String -> CCode a -> CCode a
    IfNDefine    :: String -> CCode a -> CCode a
    HashDefine   :: String -> CCode Toplevel
    Statement    :: UsableAs e Expr => CCode e -> CCode Stat
    Switch       :: (UsableAs e Expr) => CCode e -> [(CCode Name, CCode Stat)] -> CCode Stat -> CCode Stat
    StructDecl   :: CCode Ty -> [CVarSpec] -> CCode Toplevel
    Struct       :: CCode Name -> CCode Ty
    Record       :: UsableAs e Expr => [CCode e] -> CCode Expr
    Assign       :: (UsableAs l Lval, UsableAs e Expr) => CCode l -> CCode e -> CCode Stat
    AssignTL     :: (UsableAs l Lval, UsableAs e Expr) => CCode l -> CCode e -> CCode Toplevel
    Decl         :: CVarSpec -> CCode Lval
    DeclTL       :: CVarSpec -> CCode Toplevel
    Concat       :: [CCode Toplevel] -> CCode Toplevel
    Seq          :: UsableAs Stat s => [CCode s] -> CCode Stat
    Enum         :: [CCode Name] -> CCode Toplevel
    Braced       :: CCode a -> CCode a
    Parens       :: CCode a -> CCode a
    CUnary       :: UsableAs e Expr => CCode Name -> CCode e -> CCode Expr
    BinOp        :: UsableAs e Expr => CCode Name -> CCode e -> CCode e -> CCode Expr
    Dot          :: (UsableAs e Expr) => CCode e -> CCode Name -> CCode Lval
    Arrow        :: (UsableAs e Expr) => CCode e -> CCode Name -> CCode Lval
    Deref        :: UsableAs e Expr => CCode e -> CCode e
    Cast         :: UsableAs e Expr => CCode Ty -> CCode e -> CCode Expr
    ArrAcc       :: Int -> CCode Lval -> CCode Lval
    Amp          :: (UsableAs e Expr) => CCode e -> CCode Expr -- Ampersand
    Ptr          :: CCode Ty -> CCode Ty
    FunctionDecl :: CCode Ty -> CCode Name -> [CCode Ty] -> CCode Toplevel
    Function     :: CCode Ty -> CCode Name -> [CVarSpec] -> CCode Stat -> CCode Toplevel
    AsExpr       :: CCode Lval -> CCode Expr
    AsLval       :: CCode Name -> CCode Lval
    AsType       :: CCode Name -> CCode Ty
    Nam          :: String -> CCode Name
    Var          :: String -> CCode Lval
    Typ          :: String -> CCode Ty
    Static       :: CCode Ty -> CCode Ty
    Extern       :: CCode Ty -> CCode Ty
    Embed        :: String -> CCode a
    EmbedC       :: CCode a -> CCode b
    Call         :: (UsableAs e1 Expr, UsableAs e2 Expr) => CCode e1 -> [CCode e2] -> CCode Expr
    Typedef      :: CCode Ty -> CCode Name -> CCode Toplevel
    Sizeof       :: CCode Ty -> CCode Expr
    FunTypeDef   :: CCode Name -> CCode Ty -> [CCode Ty] -> CCode Toplevel
    While        :: CCode Expr -> CCode Stat -> CCode Stat
    StatAsExpr   :: CCode Lval -> CCode Stat -> CCode Expr
    If           :: UsableAs e Expr => CCode e -> CCode Stat -> CCode Stat -> CCode Expr
    Ternary      :: UsableAs e Expr => CCode e -> CCode Expr -> CCode Expr -> CCode Expr
    Return       :: UsableAs e Expr => CCode e -> CCode Stat
    UnionInst    :: UsableAs e Expr => CCode Name -> CCode e -> CCode Expr
    Int          :: Int -> CCode Expr
    Char         :: Char -> CCode Expr
    String       :: String -> CCode Expr
    Double       :: Double -> CCode Expr
    Comm         :: String -> CCode a
    Annotated    :: String -> CCode a -> CCode a
    FunPtrDecl   :: CCode Ty -> CCode Name -> [CCode Ty] -> CCode Stat
    CompoundLiteral :: CCode Ty -> [(CCode Lval, CCode Expr)] -> CCode Expr
    DesignatedInitializer :: [(CCode Name, CCode Expr)] -> CCode Expr
