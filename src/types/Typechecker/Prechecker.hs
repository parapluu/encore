{-|

Typechecks an "AST.AST" and produces the same tree, extended with
type information in the interfaces of functions, classes and
traits (it does not touch expressions at all). It throws an
exception with a meaningful error message if it fails.

-}

module Typechecker.Prechecker(precheckEncoreProgram) where

import Control.Monad.Reader
import Control.Monad.Except
import Data.Maybe

-- Module dependencies
import AST.AST hiding (hasType, getType)
import qualified AST.AST as AST
import Identifiers
import Types
import Typechecker.Environment
import Typechecker.TypeError
import Typechecker.Util

-- | The top-level type checking function
precheckEncoreProgram :: Program -> Either TCError Program
precheckEncoreProgram p = do
  env <- buildEnvironment p
  runReader (runExceptT (doPrecheck p)) env

class Precheckable a where
    doPrecheck :: a -> ExceptT TCError (Reader Environment) a

    precheck :: Pushable a => a -> ExceptT TCError (Reader Environment) a
    precheck x = local (pushBT x) $ doPrecheck x

instance Precheckable Program where
    doPrecheck p@Program{imports, functions, traits, classes} = do
      assertDistinctness
      imports'   <- mapM precheck imports
      functions' <- mapM precheck functions
      traits'    <- mapM precheck traits
      classes'   <- mapM precheck classes
      return p{imports = imports'
              ,functions = functions'
              ,traits = traits'
              ,classes = classes'
              }
      where
      assertDistinctness = do
        assertDistinct "definition" (allFunctions p)
        assertDistinct "definition" (allTraits p)
        assertDistinct "definition" (allClasses p)
        assertDistinctThing "declaration" "class or trait name" $
                            map (getId . tname) (allTraits p) ++
                            map (getId . cname) (allClasses p)

instance Precheckable ImportDecl where
    doPrecheck i@PulledImport{iprogram} = do
      iprogram' <- doPrecheck iprogram
      return i{iprogram = iprogram'}
    doPrecheck Import{} =
      error "Prechecker.hs: Import AST Nodes should not exist during typechecking"

instance Precheckable Function where
    doPrecheck f@Function{funtype, funparams} = do
      funtype'   <- resolveType funtype
      funparams' <- mapM precheck funparams
      return $ setType funtype' f{funparams = funparams'}

instance Precheckable ParamDecl where
    doPrecheck p@Param{ptype} = do
      ptype' <- resolveType ptype
      when (isRefType ptype') $
           assertResolvedModes ptype'
      return $ setType ptype' p

instance Precheckable TraitDecl where
    doPrecheck t@Trait{tname, tfields, tmethods} = do
      assertDistinctness
      tname'    <- local addTypeParams $ resolveType tname
      tfields'  <- mapM (local addTypeParams . precheck) tfields
      tmethods' <- mapM (local (addTypeParams . addThis) . precheck) tmethods
      return $ setType tname' t{tfields = tfields', tmethods = tmethods'}
      where
        typeParameters = getTypeParameters tname
        addTypeParams = addTypeParameters typeParameters
        addThis = extendEnvironment [(thisName, tname)]
        assertDistinctness = do
          assertDistinctThing "declaration" "type parameter" typeParameters
          assertDistinct "requirement" tfields
          assertDistinct "definition" tmethods

instance Precheckable ClassDecl where
    doPrecheck t@Class{cname, cfields, cmethods} = do
      assertDistinctness
      cname'    <- local addTypeParams $ resolveType cname
      cfields'  <- mapM (local addTypeParams . precheck) cfields
      cmethods' <- mapM (local (addTypeParams . addThis) . precheck) cmethods
      assertResolvedTraits cname'
      assertResolvedModes cname'
      return $ setType cname' t{cfields = cfields', cmethods = cmethods'}
      where
        typeParameters = getTypeParameters cname
        addTypeParams = addTypeParameters typeParameters
        addThis = extendEnvironment [(thisName, cname)]
        assertDistinctness = do
          assertDistinctThing "declaration" "type parameter" typeParameters
          assertDistinctThing "inclusion" "trait" $ getImplementedTraits cname
          assertDistinct "declaration" cfields
          assertDistinct "declaration" cmethods

instance Precheckable FieldDecl where
    doPrecheck f@Field{ftype} = do
      ftype' <- resolveType ftype
      return $ setType ftype' f

instance Precheckable MethodDecl where
    doPrecheck m = do
      mtype'   <- resolveType $ mtype m
      mparams' <- mapM precheck $ mparams m
      thisType <- liftM fromJust $ asks $ varLookup thisName
      when (isMainMethod thisType (mname m)) checkMainParams
      when (isStreamMethod m) $
           unless (isActiveClassType thisType) $
                  tcError "Cannot have streaming methods in a passive class"
      return $ setType mtype' m{mparams = mparams'}
      where
        checkMainParams =
            unless (map ptype (mparams m) `elem` [[], [arrayType stringType]]) $
              tcError "Main method must have argument type () or ([string])"
