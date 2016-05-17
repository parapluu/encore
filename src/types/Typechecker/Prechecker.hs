{-|

Typechecks an "AST.AST" and produces the same tree, extended with
type information in the interfaces of functions, classes and
traits (it does not touch expressions at all). It throws an
exception with a meaningful error message if it fails.

-}

module Typechecker.Prechecker(precheckEncoreProgram) where

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
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
precheckEncoreProgram :: Environment -> Program -> (Either TCError Program, [TCWarning])
precheckEncoreProgram env p =
  case buildEnvironment env p of
    (Right env, warnings) -> do
      let readerVar = runReaderT (doPrecheck p) env
      let exceptVar = runExceptT readerVar
      runState exceptVar warnings
    (Left err, warnings) -> (Left err, warnings)

class Precheckable a where
    doPrecheck :: a -> TypecheckM a

    precheck :: Pushable a => a -> TypecheckM a
    precheck x = local (pushBT x) $ doPrecheck x

instance Precheckable Program where
    doPrecheck p@Program{typedefs, functions, traits, classes} = do
      assertDistinctness
      typedefs'   <- mapM precheck typedefs
      functions' <- mapM precheck functions
      traits'    <- mapM precheck traits
      classes'   <- mapM precheck classes
      return p{typedefs = typedefs'
              ,functions = functions'
              ,traits = traits'
              ,classes = classes'
              }
      where
      assertDistinctness = do
        assertDistinct "definition" (allFunctions p)
        assertDistinct "definition" (allTraits p)
        assertDistinct "definition" (allClasses p)
        assertDistinctThing "declaration" "typedef, class or trait name" $
                            map (getId . tname) (allTraits p) ++
                            map (getId . cname) (allClasses p) ++
                            map (getId . typedefdef) (allTypedefs p)

instance Precheckable Typedef where
   doPrecheck t@Typedef{typedefdef} = do
     let resolvesTo = typeSynonymRHS typedefdef
         addTypeParams = addTypeParameters $ getTypeParameters typedefdef
     resolvesTo' <- local addTypeParams $ resolveTypeAndCheckForLoops resolvesTo
     return $ t{typedefdef = typeSynonymSetRHS typedefdef resolvesTo'}

instance Precheckable FunctionHeader where
    doPrecheck header = do
      let htypeparams' = htypeparams header
      htype' <- local (addTypeParameters htypeparams') $ resolveType (htype header)
      hparams' <- local (addTypeParameters htypeparams') $ mapM precheck (hparams header)
      assertDistinctThing "declaration" "type parameter" htypeparams'
      assertDistinctThing "definition" "parameter" $ map pname hparams'
      return $ header{htype = htype',
                      hparams = hparams',
                      htypeparams= htypeparams'}

instance Precheckable Function where
    doPrecheck f@Function{funheader} = do
      funheader' <- doPrecheck funheader
      let funtype = htype funheader'
      return $ setType funtype f{funheader = funheader'}

instance Precheckable ParamDecl where
    doPrecheck p@Param{ptype} = do
      ptype' <- resolveType ptype
      return $ setType ptype' p

instance Precheckable Requirement where
    doPrecheck req
        | isRequiredField req = do
            rfield' <- doPrecheck $ rfield req
            let ty = AST.getType rfield'
            return $ setType ty req{rfield = rfield'}
        | isRequiredMethod req = do
            rheader' <- doPrecheck $ rheader req
            let ty = htype rheader'
            return $ setType ty req{rheader = rheader'}
        | otherwise =
            error $ "Prechecker.hs: requirement '" ++ show req ++
                    "' is neither a field, nor a method"

instance Precheckable TraitDecl where
    doPrecheck t@Trait{tname, treqs, tmethods} = do
      assertDistinctness
      treqs'    <- mapM (local (addTypeParams . addThis) . precheck) treqs
      tmethods' <- mapM (local (addTypeParams . addThis) . precheck) tmethods
      return $ setType tname t{treqs = treqs', tmethods = tmethods'}
      where
        typeParameters = getTypeParameters tname
        addTypeParams = addTypeParameters typeParameters
        addThis = extendEnvironment $ if isModeless tname
                                      then [(thisName, makeSubordinate tname)]
                                      else [(thisName, tname)]
        assertDistinctness = do
          assertDistinctThing "declaration" "type parameter" typeParameters
          assertDistinct "requirement" treqs
          assertDistinct "definition" tmethods

instance Precheckable ClassDecl where
    doPrecheck c@Class{cname, ccapability, cfields, cmethods} = do
      assertDistinctness
      cname'       <- local addTypeParams $ resolveType cname
      ccapability' <- local addTypeParams $ resolveType ccapability
      cfields'     <- mapM (local (addTypeParams . addThis) . precheck) cfields
      cmethods'    <- mapM (local (addTypeParams . addThis) . precheck) cmethods
      return $ setType cname' c{ccapability = ccapability'
                               ,cfields = cfields'
                               ,cmethods = if any isConstructor cmethods'
                                           then cmethods'
                                           else emptyConstructor c : cmethods'
                               }
      where
        typeParameters = getTypeParameters cname
        addTypeParams = addTypeParameters typeParameters
        addThis = extendEnvironment [(thisName, cname)]
        assertDistinctness = do
            assertDistinctThing "declaration" "type parameter" typeParameters
            assertDistinctThing "inclusion" "trait" $
                                typesFromCapability ccapability
            assertDistinct "declaration" cfields
            assertDistinct "declaration" cmethods

instance Precheckable FieldDecl where
    doPrecheck f@Field{ftype} = do
      ftype' <- resolveType ftype
      thisType <- liftM fromJust . asks . varLookup $ thisName
      when (isReadRefType thisType) $ do
           unless (isValField f) $
                  tcError NonValInReadTraitError
           unless (isSafeType ftype') $
                  tcError $ NonSafeInReadTraitError ftype'
      return $ setType ftype' f

instance Precheckable MethodDecl where
    doPrecheck m@Method{mheader} = do
      mheader' <- doPrecheck mheader
      thisType <- liftM fromJust $ asks $ varLookup thisName
      when (isMainMethod thisType (methodName m))
           (checkMainParams $ hparams mheader')
      when (isStreamMethod m) $ do
           unless (isActiveClassType thisType) $
                  tcError PassiveStreamingMethodError
           when (isConstructor m) $
                tcError StreamingConstructorError
      let mtype = htype mheader'
      return $ setType mtype m{mheader = mheader'}
      where
        checkMainParams params =
            unless (map ptype params `elem` allowedMainArguments) $
              tcError MainMethodArgumentsError
        allowedMainArguments = [[], [arrayType stringObjectType]]
