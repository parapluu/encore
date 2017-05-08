module CodeGen.Preprocessor(preprocess) where

import Data.List
import Data.Maybe

import qualified AST.AST as A
import qualified AST.Util as Util
import qualified AST.Meta as Meta
import qualified Types as Ty

-- | Functions run on the entire AST before code generation.
preprocess :: A.Program -> A.Program
preprocess = injectTraitsToClasses . giveClosuresUniqueNames

injectTraitsToClasses :: A.Program -> A.Program
injectTraitsToClasses p =
  Util.mapProgramClass p injectTraitsToClass
  where
    injectTraitsToClass :: A.ClassDecl -> A.ClassDecl
    injectTraitsToClass c@A.Class{A.ccomposition} =
        foldr injectTraitToClass c (A.typesFromTraitComposition ccomposition)

    injectTraitToClass :: Ty.Type -> A.ClassDecl -> A.ClassDecl
    injectTraitToClass traitType c@A.Class{A.cmethods} =
        let
            traitTemplate = A.getTrait traitType p
            injectedMethods = flattenTrait c traitType traitTemplate
        in
          c{A.cmethods = cmethods ++ injectedMethods}

-- | @flattenTrait cdecl t tdecl@ returns the methods of
-- @tdecl@, translated to appear as if they were declared in
-- @cdecl@ with any type parameters of @tdecl@ instantiated to the
-- type arguments of @t@.
flattenTrait :: A.ClassDecl -> Ty.Type -> A.TraitDecl -> [A.MethodDecl]
flattenTrait cdecl traitType template =
  let
    formals = Ty.getTypeParameters $ A.tname template
    actuals = Ty.getTypeParameters traitType
    bindings = zip formals actuals
    traitMethods = A.tmethods template
    classMethods = map A.methodName $ A.cmethods cdecl
    nonOverridden = filter ((`notElem` classMethods) . A.methodName) traitMethods
    nonOverridden' = map alphaConvertTypeArgs nonOverridden
  in
    map (convertMethod bindings cdecl) nonOverridden'

-- | Make type parameters of a method unique to avoid name clashes
-- when flattening traits
alphaConvertTypeArgs :: A.MethodDecl -> A.MethodDecl
alphaConvertTypeArgs method =
  let
    mheader     = A.mheader method
    typeParams  = A.htypeparams mheader
    htypeparams = map alphaConvert typeParams
    bindings    = zip typeParams htypeparams
    mheader'    = A.replaceHeaderTypes bindings mheader{A.htypeparams}
    mbody'      = Util.extend (Util.exprTypeMap (Ty.replaceTypeVars bindings))
                              (A.mbody method)
  in method{A.mheader = mheader', A.mbody = mbody'}
  where
    alphaConvert ty =
      let id = Ty.getId ty
          id' = "_" ++ id
      in Ty.typeVar id'

-- | @convertMethod bindings cdecl m@ converts all types
-- appearing in @m@ using @bindings@ as a convertion table. It
-- also gives all accesses of or through @this@ types as if
-- @cdecl@ was the current enclosing class.
convertMethod ::
    [(Ty.Type, Ty.Type)] -> A.ClassDecl -> A.MethodDecl -> A.MethodDecl
convertMethod bindings cdecl method =
  let
    header = A.mheader method
    htype = convertType $ A.methodType method
    hparams = map convertNode $ A.methodParams method
    mheader = header{A.htype, A.hparams}
    mbody = Util.extend convertExpr $ A.mbody method
  in
    method{A.mheader, A.mbody}
  where
    convertType :: Ty.Type -> Ty.Type
    convertType = Ty.replaceTypeVars bindings

    convertExpr :: A.Expr -> A.Expr
    convertExpr e
        | A.isThisAccess e = A.setType (A.cname cdecl) $ convertNode e
        | isThisFieldAccess e =
            let f = A.name e
                ty = getFieldType f (A.cfields cdecl)
            in A.setType ty $ convertNode e
        | otherwise = convertNode $ Util.exprTypeMap convertType e

    isThisFieldAccess A.FieldAccess{A.target} = A.isThisAccess target
    isThisFieldAccess _ = False

    getFieldType f =
        A.ftype . fromMaybe err . find ((== f) . A.fname)
            where
              err = error $ "Preprocessor.hs: Could not find field: " ++ show f

    convertNode :: A.HasMeta n => n -> n
    convertNode node =
      let
        oldType = A.getType node
        newType = convertType oldType
      in
        A.setType newType node

giveClosuresUniqueNames :: A.Program -> A.Program
giveClosuresUniqueNames = snd . Util.extendAccumProgram uniqueClosureName 0
    where
      uniqueClosureName acc e
          | A.isClosure e =
              let m = A.getMeta e
                  mclosure = Meta.metaClosure ("closure" ++ show acc) m
              in (acc + 1, A.setMeta e mclosure)
          | otherwise = (acc, e)
