{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE MultiParamTypeClasses,FlexibleInstances #-}

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
import AST.Util(exprTypeMap, extend)
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
      assertNonOverlapWithPredefineds
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
        assertDistinctThing "declaration" "typedef, class or trait name" $ newTypeNames
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
      assertNonOverlapWithPredefineds = 
        when (or (map (`elem` ["Maybe", "Fut", "Stream", "Par"]) newTypeNames)) $
                tcError OverlapWithBuiltins 
      newTypeNames = map (getId . tname) traits ++
                     map (getId . cname) classes ++
                     map (getId . typedefdef) typedefs

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

instance Precheckable Typedef where
   doPrecheck t@Typedef{typedefdef} = do
     let typeParams = getTypeParameters typedefdef
     typeParams' <- mapM resolveTypeParameter typeParams
     let resolvesTo = typeSynonymRHS typedefdef
         addTypeParams = addTypeParameters typeParams'
     resolvesTo' <- local addTypeParams $ resolveTypeAndCheckForLoops resolvesTo
     let typedefdef' = typeSynonymSetRHS typedefdef resolvesTo'
                       `setTypeParameters` typeParams'
     return $ t{typedefdef = typedefdef'}

instance Precheckable FunctionHeader where
    doPrecheck header = do
      htypeparams' <- mapM resolveTypeParameter (htypeparams header)
      htype' <- local (addTypeParameters htypeparams') $
                      resolveType (htype header)
      hparams' <- local (addTypeParameters htypeparams') $
                        mapM precheck (hparams header)

      classTypeParams <- asks typeParameters
      assertDistinctThing "declaration" "type parameter" $
                          map (typeVar . getId)
                              (htypeparams' ++ classTypeParams)
      assertDistinctThing "definition" "parameter" $ map pname hparams'
      return $ header{htypeparams = htypeparams',
                      htype = htype',
                      hparams = hparams'}

resolveTypeParameter ty
  | Just bound <- getBound ty = do
      -- TODO: Check modes
      bound' <- resolveType bound
      unless (isCapabilityType bound') $
             tcError $ MalformedBoundError bound'
      return $ setBound (Just bound') ty
  | otherwise = return ty

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
      typeParams <- mapM resolveTypeParameter (getTypeParameters tname)
      when (isSharableSingleType tname) $
           tcError $ CannotGiveSharableModeError tname
      treqs'    <- mapM (local (addTypeParameters typeParams) . doPrecheck)
                        treqs
      tmethods' <- mapM (local (addTypeParameters typeParams .
                                addMinorThis tname) . precheck)
                        tmethods
      let tmethods'' = map alphaConvertMethod tmethods'
      return $ setType (setTypeParameters tname typeParams)
               t{treqs = treqs', tmethods = tmethods''}
      where
        addMinorThis self =
            extendEnvironmentImmutable $
              if hasMinorMode tname
              then [(thisName, makeSubordinate self)]
              else [(thisName, self)]
        assertDistinctness = do
          assertDistinctThing "declaration" "type parameter" $
                              map (typeVar . getId) $ getTypeParameters tname
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
      Just (_, thisType) <- findVar (qLocal thisName)

      formal <- findFormalRefType tcname
      tcname' <- if isModeless thisType || not (isTraitType formal) ||
                    isTraitType formal && not (hasMinorMode formal) ||
                    isTraitType tcname && not (isModeless tcname)
                 then resolveType tcname
                 else resolveType $ tcname `withModeOf` thisType

      let (left, right) = getTypeOperands tcname'
          tcleft = TraitLeaf{tcname = left, tcext}
          tcright = TraitLeaf{tcname = right, tcext}
      if isConjunctiveType tcname' then
          doPrecheck Conjunction{tcleft, tcright}
      else if isDisjunctiveType tcname' then
          doPrecheck Disjunction{tcleft, tcright}
      else if isTypeSynonym formal then -- recheck to unfold type synonym
          doPrecheck leaf{tcname = tcname'}
      else do
        unless (safeToComposeWith thisType tcname') $
               tcError $ ManifestClassConflictError
                         thisType tcname'
        mapM_ doPrecheck tcext
        return leaf{tcname = tcname'}

    doPrecheck comp = do
      tcleft' <- doPrecheck $ tcleft comp
      tcright' <- doPrecheck $ tcright comp
      return comp{tcleft = tcleft', tcright = tcright'}

instance Precheckable ClassDecl where
    doPrecheck c@Class{cname, ccomposition, cfields, cmethods} = do
      assertDistinctness
      typeParams <- mapM resolveTypeParameter (getTypeParameters cname)
      cname' <- local (addTypeParameters typeParams) $
                      resolveType (setTypeParameters cname typeParams)

      ccomposition' <- checkComposition cname'
      cfields' <- mapM (local (addTypeParameters typeParams . addThis cname') .
                        precheck) cfields
      cmethods' <- mapM (local (addTypeParameters typeParams . addThis cname') .
                         precheck) cmethods

      checkShadowingMethodsAndFields cfields' cmethods'

      let traits = typesFromTraitComposition ccomposition'
          (activeTraits, nonActiveTraits) = partition isActiveSingleType traits
      unless (null activeTraits) $
        unless (null nonActiveTraits) $
          tcError $ ActiveTraitError (head activeTraits) (head nonActiveTraits)

      return $ setType cname' c{ccomposition = ccomposition'
                               ,cfields = cfields'
                               ,cmethods = if any isConstructor cmethods'
                                           then cmethods'
                                           else emptyConstructor c : cmethods'
                               }
      where
        addThis self = extendEnvironmentImmutable [(thisName, self)]
        assertDistinctness = do
            assertDistinctThing "declaration" "type parameter" $
                                map (typeVar . getId) $ getTypeParameters cname
            assertDistinctThing "inclusion" "trait" $
                                typesFromTraitComposition ccomposition
            assertDistinct "declaration" cfields
            assertDistinct "declaration" cmethods

        checkComposition thisType = do
          let capability = capabilityFromTraitComposition ccomposition
              resolveFormally t | isRefAtomType t = findFormalRefType t
                                | otherwise = return t
              typeParams = getTypeParameters thisType
          resolvedCapability <- local (addTypeParameters typeParams) $
                                  typeMapM resolveFormally capability

          -- The resolving above is needed to allow looking up
          -- attributes in the traits when doing the actual
          -- resolving of the trait composition below.
          case ccomposition of
            Just composition -> do
              composition' <-
                local (addTypeParameters typeParams . addThis thisType .
                       setTraitComposition thisType resolvedCapability) $
                    doPrecheck composition
              return $ Just composition'
            Nothing -> return Nothing

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

      when (isMainType thisType && isConstructor m) $
           unless (isImplicitMethod m) $
                  tcError MainConstructorError

      when (isStreamMethod m) $ do
           unless (isActiveSingleType thisType) $
                  tcError PassiveStreamingMethodError
           when (isConstructor m) $
                tcError StreamingConstructorError
      when (isConstructor m) $
            unless((null . methodTypeParams) m) $
              tcError PolymorphicConstructorError
      let typeParams = htypeparams mheader'
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

alphaConvertMethod m@Method{mheader, mbody, mlocals} =
  let typeParams    = htypeparams mheader
      bindings      = map buildBinding typeParams
      htypeparams'  = map (replaceTypeVars bindings) typeParams
      mheader'      = replaceHeaderTypes bindings
                      mheader{htypeparams = htypeparams'}
      mbody'        = convertExpr bindings mbody
      mlocals'      = map (convertLocal bindings) mlocals
  in m{mheader = mheader', mbody = mbody', mlocals = mlocals'}
  where
    buildBinding ty =
      let id = getId ty
          id' = "_" ++ id
      in (ty, alphaConvert id' ty)

    convertExpr bindings = extend (exprTypeMap (replaceTypeVars bindings))

    convertLocal bindings f@Function{funheader, funbody} =
      let funheader' = replaceHeaderTypes bindings funheader
          funbody'   = convertExpr bindings funbody
      in f{funheader = funheader', funbody = funbody'}