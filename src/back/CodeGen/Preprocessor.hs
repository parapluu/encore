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
injectTraitsToClasses p@A.Program{A.classes} =
  p{A.classes = map injectTraitsToClass classes}
  where
    injectTraitsToClass :: A.ClassDecl -> A.ClassDecl
    injectTraitsToClass c@A.Class{A.cname} =
        foldr injectTraitToClass c (Ty.getImplementedTraits cname)

    injectTraitToClass :: Ty.Type -> A.ClassDecl -> A.ClassDecl
    injectTraitToClass traitType c@A.Class{A.cname, A.cmethods} =
        let
            traitTemplate = A.getTrait traitType p
            injectedMethods = flattenTrait cname traitType traitTemplate
        in
          c{A.cmethods = cmethods ++ injectedMethods}

-- | @flattenTrait c t tdecl@ returns the methods of
-- @tdecl@, translated to appear as if they were declared in class
-- @c@ with any type parameters of @tdecl@ instantiated to the
-- type arguments of @t@.
flattenTrait :: Ty.Type -> Ty.Type -> A.TraitDecl -> [A.MethodDecl]
flattenTrait c traitType template =
  let
    formals = Ty.getTypeParameters $ A.tname template
    actuals = Ty.getTypeParameters traitType
    bindings = zip formals actuals
    methods = A.tmethods template
  in
    map (convertMethod bindings c) methods

-- | @convertMethod bindings thisType m@ converts all types
-- appearing in @m@ using @bindings@ as a convertion table, and
-- also gives the type @thisType@ to all accesses of @this@.
convertMethod ::
    [(Ty.Type, Ty.Type)] -> Ty.Type -> A.MethodDecl -> A.MethodDecl
convertMethod bindings thisType method =
  let
    mtype = convertType $ A.mtype method
    mparams = map convertNode $ A.mparams method
    mbody = Util.extend convertExpr $ A.mbody method
  in
    method{A.mtype, A.mparams, A.mbody}
  where
    convertType :: Ty.Type -> Ty.Type
    convertType = Ty.typeMap (\ty -> fromMaybe ty (lookup ty bindings))

    convertExpr :: A.Expr -> A.Expr
    convertExpr e
        | A.isThisAccess e = A.setType thisType $ convertNode e
        | otherwise = convertNode e

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
          | A.isTask e =
              let m = A.getMeta e
                  mtask = Meta.metaTask ("task" ++ show acc) m
              in (acc + 1, A.setMeta e mtask)
          | otherwise = (acc, e)
