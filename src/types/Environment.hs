module Environment(Environment, 
                   EnvironmentTransformer,
                   buildClassTable, 
                   classLookup, 
                   methodLookup, 
                   fieldLookup, 
                   varLookup,
                   extendEnvironment,
                   Backtrace,
                   pushBT,
                   backtrace) where

import Data.Maybe

import Types
import AST
import PrettyPrinter
import Text.PrettyPrint

type Backtrace = [BacktraceNode]

data BacktraceNode = BTClass Type | BTParam ParamDecl | BTField FieldDecl | BTMethod Name | BTExpr Expr deriving(Eq)

instance Show BacktraceNode where
    show (BTClass (Type t))  = "In class '"        ++ t                      ++ "'"
    show (BTParam p)         = "In parameter '"    ++ (show $ ppParamDecl p) ++ "'"
    show (BTField f)         = "In field '"        ++ (show $ ppFieldDecl f) ++ "'"
    show (BTMethod (Name n)) = "In method '"       ++ n                      ++ "'"
    show (BTExpr expr)       = "In expression: \n" ++ (show $ nest 2 $ ppExpr expr)

class Pushable a where
    pushBT :: a -> EnvironmentTransformer
instance Pushable ClassDecl where
    pushBT (Class cname _ _) (Env ctable locals bt) = Env ctable locals ((BTClass cname):bt)
instance Pushable FieldDecl where
    pushBT f (Env ctable locals bt) = Env ctable locals ((BTField f):bt)
instance Pushable ParamDecl where
    pushBT p (Env ctable locals bt) = Env ctable locals ((BTParam p):bt)
instance Pushable MethodDecl where
    pushBT (Method name _ _ _) (Env ctable locals bt) = Env ctable locals ((BTMethod name):bt)
instance Pushable Expr where
    pushBT expr (Env ctable locals bt) = Env ctable locals ((BTExpr expr):bt)

backtrace (Env _ _ bt) = bt

type ClassTable = [ClassType]
data Environment = Env ClassTable [VarType] Backtrace

type EnvironmentTransformer = Environment -> Environment

buildClassTable :: Program -> Environment
buildClassTable (Program classes) = Env (map getClassType classes) [] []

getClassType :: ClassDecl -> ClassType
getClassType (Class name fields methods) = (name, (fields', methods'))
    where
      fields' = map getFieldType fields
      methods' = map getMethodType methods
      getFieldType (Field name ty) = (name, ty)
      getMethodType (Method name rtype params _) = (name, (rtype, params))

fieldLookup :: Type -> Name -> Environment -> Maybe Type
fieldLookup cls f (Env ctable _ _) = do (fields, _) <- lookup cls ctable
                                        lookup f fields

methodLookup :: Type -> Name -> Environment -> Maybe (Type, [ParamDecl])
methodLookup cls m (Env ctable _ _) = do (_, methods) <- lookup cls ctable
                                         lookup m methods

classLookup :: Type -> Environment -> Maybe ([FieldType], [MethodType])
classLookup cls (Env ctable _ _) = lookup cls ctable

varLookup :: Name -> Environment -> Maybe Type
varLookup x (Env _ locals _) = lookup x locals

extendEnvironment :: [(Name, Type)] -> Environment -> Environment
extendEnvironment [] env = env
extendEnvironment ((name, ty):newTypes) (Env ctable locals bt) = 
    extendEnvironment newTypes (Env ctable (extend locals name ty) bt)
    where
      extend [] name' ty' = [(name', ty')]
      extend ((name, ty):bindings) name' ty'
          | name == name' = (name', ty'):bindings
          | otherwise     = (name, ty):(extend bindings name' ty')