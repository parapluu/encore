{-# LANGUAGE ConstrainedClassMethods #-}

{-|

Typechecks an "AST.AST" and produces the same tree, extended with
type information in the interfaces of functions, classes and
traits (it does not touch expressions at all). It throws an
exception with a meaningful error message if it fails.

-}

module Typechecker.Prechecker(precheckProgram) where

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import Data.Maybe
import Data.List
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import SystemUtils

import Debug.Trace

-- Module dependencies
import AST.AST hiding (hasType, getType)
import qualified AST.AST as AST
import Identifiers
import Types
import Typechecker.Environment
import Typechecker.TypeError
import Typechecker.Util

-- | The top-level type checking function
precheckProgram :: Map FilePath LookupTable -> Program ->
                   (Either TCError Program, [TCWarning])
precheckProgram table p =
  let env = buildEnvironment table p
      readerVar = runReaderT (doPrecheck p) env
      exceptVar = runExceptT readerVar
  in runState exceptVar []

class Precheckable a where
    doPrecheck :: a -> TypecheckM a

    precheck :: Pushable a => a -> TypecheckM a
    precheck x = local (pushBT x) $ doPrecheck x

instance Precheckable Program where
    doPrecheck p@Program{source
                        ,moduledecl
                        ,imports
                        ,typedefs
                        ,functions
                        ,traits
                        ,classes} = do
      assertDistinctness
      precheck moduledecl
      assertCorrectModuleName source moduledecl
      assertNoShadowedImports
      mapM_ precheck imports
      typedefs'  <- mapM precheck typedefs
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
        assertDistinct "definition" functions
        assertDistinct "definition" traits
        assertDistinct "definition" classes
        assertDistinctThing "declaration" "typedef, class or trait name" $
                            map (getId . tname) traits ++
                            map (getId . cname) classes ++
                            map (getId . typedefdef) typedefs
      assertCorrectModuleName _ NoModule = return ()
      assertCorrectModuleName source m@Module{modname} = do
        let sourceName = basename source
            expectedModuleName = Name $ dropSuffix sourceName
        unless (modname == expectedModuleName) $
               pushError m $ WrongModuleNameError modname sourceName
      assertNoShadowedImports = do
        let importAliases = mapMaybe ialias imports
        assertDistinctThing "usage" "module alias"
                            (map show importAliases)
        let shadowedImports =
              filter (\i -> itarget i /= fromMaybe emptyNamespace (ialias i)) $
                     filter ((`elem` importAliases) . itarget) imports
        unless (null shadowedImports) $ do
          let shadowedImport = head shadowedImports
              illegalAlias =
                fromJust $
                  find ((== Just (itarget shadowedImport)) . ialias) imports
          pushError illegalAlias $ ShadowedImportError shadowedImport


instance Precheckable ModuleDecl where
    doPrecheck m@NoModule = return m
    doPrecheck m@Module{modname, modexports} = do
      env <- ask
      let modNamespace = explicitNamespace [modname]
          unknowns =
            maybe [] (filter (\x -> not $ isKnownName modNamespace x env))
                     modexports
      unless (null unknowns) $
             tcError $ UnknownNameError modNamespace (head unknowns)
      return m

instance Precheckable ImportDecl where
    doPrecheck i@Import{itarget, iselect, ihiding, ialias} = do
      env <- ask
      let unknownSelects =
            maybe [] (filter (\x -> not $ isKnownName itarget x env)) iselect
          unknownHiding =
            maybe [] (filter (\x -> not $ isKnownName itarget x env)) ihiding
          unknowns = unknownSelects ++ unknownHiding
      unless (null unknowns) $
             tcError $ UnknownNameError itarget (head unknowns)
      return i

-- | 'assertTypeParams ts' asserts that the types passed in are
-- all type variables and are not concrete types, and otherwise
-- throws an error.
assertTypeParams :: (MonadReader Environment f, MonadError TCError f) =>
                    [Type] -> f ()
assertTypeParams ts =
  unless (all isTypeVar ts) $
    let concreteTypes = filter (not . isTypeVar) ts
        concreteType = head concreteTypes
    in tcError $ ConcreteTypeParameterError concreteType


instance Precheckable Typedef where
   doPrecheck t@Typedef{typedefdef} = do
     let typeParams = getTypeParameters typedefdef
     assertTypeParams typeParams
     let resolvesTo = typeSynonymRHS typedefdef
         addTypeParams = addTypeParameters typeParams
     resolvesTo' <- local addTypeParams $ resolveTypeAndCheckForLoops resolvesTo
     return $ t{typedefdef = typeSynonymSetRHS typedefdef resolvesTo'}

instance Precheckable FunctionHeader where
    doPrecheck header = do
      let typeParams = htypeparams header
      assertTypeParams typeParams
      htype' <- local (addTypeParameters typeParams) $
                      resolveType (htype header)
      hparams' <- local (addTypeParameters typeParams) $
                        mapM precheck (hparams header)

      classTypeParams <- getClassTypeParams
      assertDistinctThing "declaration" "type parameter" $
                          typeParams ++ classTypeParams
      assertDistinctThing "definition" "parameter" $ map pname hparams'
      return $ header{htype = htype',
                      hparams = hparams'}
      where
        getClassTypeParams = do
          environment <- ask
          return $ typeParameters environment


precheckLocalFunctions :: [Function] -> [Type] -> TypecheckM [Function]
precheckLocalFunctions locals typeParams = do
  locals' <- local (addTypeParameters typeParams) $
                   mapM precheck locals
  let localNames = map functionName locals'
  assertDistinctThing "declaration" "local function" localNames
  return locals'

instance Precheckable Function where
    doPrecheck f@Function{funheader, funlocals} = do
      funheader' <- doPrecheck funheader
      let typeParams = htypeparams funheader'
      assertTypeParams typeParams
      funlocals' <- precheckLocalFunctions funlocals typeParams
      let funtype = htype funheader'
      return $ setType funtype f{funheader = funheader'
                                ,funlocals = funlocals'}

instance Precheckable ParamDecl where
    doPrecheck p@Param{ptype} = do
      ptype' <- resolveType ptype
      return $ setType ptype' p

instance Precheckable Requirement where
    doPrecheck req
        | isRequiredField req = do
            rfield' <- precheck $ rfield req
            let ty = AST.getType rfield'
            return $ req{rfield = rfield'}
        | isRequiredMethod req = do
            rheader' <- doPrecheck $ rheader req
            let ty = htype rheader'
            return $ req{rheader = rheader'}
        | otherwise =
            error $ "Prechecker.hs: requirement '" ++ show req ++
                    "' is neither a field, nor a method"

instance Precheckable TraitDecl where
    doPrecheck t@Trait{tname, treqs, tmethods} = do
      assertDistinctness
      let typeParams = getTypeParameters tname
      assertTypeParams typeParams
      tname'    <- local addTypeParams $ resolveType tname
      treqs'    <- mapM (local addTypeParams . doPrecheck) treqs
      tmethods' <- mapM (local (addTypeParams . addThis tname') . precheck) tmethods
      return $ setType tname' t{treqs = treqs', tmethods = tmethods'}
      where
        typeParameters = getTypeParameters tname
        addTypeParams = addTypeParameters typeParameters
        addThis self = extendEnvironmentImmutable [(thisName, self)]
        assertDistinctness = do
          assertDistinctThing "declaration" "type parameter" typeParameters
          assertDistinctThing "requirement" "field" $
                              map fname $ requiredFields t
          assertDistinctThing "requirement" "method" $
                              map hname $ requiredMethods t
          assertDistinct "definition" tmethods
          let allNames = map hname $ requiredMethods t ++ map mheader tmethods
          assertDistinctThing "declaration" "method" allNames

instance Precheckable TraitExtension where
    doPrecheck f@FieldExtension{extname} = do
      Just (_, thisType) <- findVar $ qLocal thisName
      findField thisType extname
      return f
    doPrecheck m@MethodExtension{extname} = do
      Just (_, thisType) <- findVar $ qLocal thisName
      findMethod thisType extname
      return m

instance Precheckable TraitComposition where
    doPrecheck leaf@TraitLeaf{tcname, tcext} = do
      tcname' <- resolveType tcname
      let (left, right) = getTypeOperands tcname'
          tcleft = TraitLeaf{tcname = left, tcext}
          tcright = TraitLeaf{tcname = right, tcext}
      if isConjunctiveType tcname' then
          doPrecheck Conjunction{tcleft, tcright}
      else if isDisjunctiveType tcname' then
          doPrecheck Disjunction{tcleft, tcright}
      else do
        mapM_ doPrecheck tcext
        return leaf{tcname = tcname'}

    doPrecheck comp = do
      tcleft' <- doPrecheck $ tcleft comp
      tcright' <- doPrecheck $ tcright comp
      return comp{tcleft = tcleft', tcright = tcright'}

instance Precheckable ClassDecl where
    doPrecheck c@Class{cname, ccomposition, cfields, cmethods} = do
      assertDistinctness
      let typeParams = getTypeParameters cname
      assertTypeParams typeParams
      cname' <- local addTypeParams $ resolveType cname
      let capability = capabilityFromTraitComposition ccomposition
      capability' <- local addTypeParams $
                           resolveType capability
      ccomposition' <- case ccomposition of
                         Just composition -> do
                           composition' <-
                             local (addTypeParams . addThis cname' .
                                    setTraitComposition cname' capability') $
                                   doPrecheck composition
                           return $ Just composition'
                         Nothing -> return Nothing

      cfields' <- mapM (local addTypeParams . precheck) cfields
      cmethods' <- mapM (local (addTypeParams . addThis cname') . precheck) cmethods
      checkShadowingMethodsAndFields cfields' cmethods'
      return $ setType cname' c{ccomposition = ccomposition'
                               ,cfields = cfields'
                               ,cmethods = if any isConstructor cmethods'
                                           then cmethods'
                                           else emptyConstructor c : cmethods'
                               }
      where
        typeParameters = getTypeParameters cname
        addTypeParams = addTypeParameters typeParameters
        addThis self = extendEnvironmentImmutable [(thisName, self)]
        assertDistinctness = do
            assertDistinctThing "declaration" "type parameter" typeParameters
            assertDistinctThing "inclusion" "trait" $
                                typesFromTraitComposition ccomposition
            assertDistinct "declaration" cfields
            assertDistinct "declaration" cmethods

        checkShadowingMethodsAndFields fields methods = do
          let shadowedFields = filter (`hasShadowIn` methods) fields
              shadowedField = head shadowedFields
          unless (null shadowedFields) $
                 pushWarning shadowedField $ ShadowedMethodWarning shadowedField
        hasShadowIn Field{fname, ftype} mdecls =
          any ((== fname) . methodName) mdecls &&
          (isArrayType ftype || isArrowType ftype)

instance Precheckable FieldDecl where
    doPrecheck f@Field{ftype} = do
      ftype' <- resolveType ftype
      return $ setType ftype' f

instance Precheckable MethodDecl where
    doPrecheck m@Method{mheader, mlocals} = do
      mheader' <- doPrecheck mheader
      Just (_, thisType) <- findVar (qLocal thisName)
      when (isMainMethod thisType (methodName m))
           (checkMainParams $ hparams mheader')
      when (isStreamMethod m) $ do
           unless (isActiveClassType thisType) $
                  tcError PassiveStreamingMethodError
           when (isConstructor m) $
                tcError StreamingConstructorError
      when (isConstructor m) $
            unless((null . methodTypeParams) m) $
              tcError PolymorphicConstructorError
      let typeParams = htypeparams mheader'
      assertTypeParams typeParams
      mlocals' <- precheckLocalFunctions mlocals typeParams

      let mtype = htype mheader'

      when (isMatchMethod m) $ do
           unless (null $ hparams mheader') $
                  tcError MatchMethodNonEmptyParameterListError
           unless (isMaybeType mtype) $
                  tcError MatchMethodNonMaybeReturnError

      return $ setType mtype m{mheader = mheader'
                              ,mlocals = mlocals'}
      where
        checkMainParams params =
            unless (allowedMainArguments $ map ptype params) $
              tcError MainMethodArgumentsError
        allowedMainArguments [] = True
        allowedMainArguments [ty] = isArrayType ty &&
                                    isStringObjectType (getResultType ty)
        allowedMainArguments _ = False
