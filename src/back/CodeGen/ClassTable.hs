module CodeGen.ClassTable (
  ProgramTable,
  lookupMethod,
  lookupMethods,
  lookupField,
  lookupCalledType,
  lookupFunction,
  buildProgramTable,
  getGlobalFunctionNames) where

import Types
import AST.AST
import Identifiers

import Data.List
import Data.Maybe
import Control.Arrow

type FieldTable  = [(Name, FieldDecl)]
type MethodTable = [(Name, FunctionHeader)]
type ClassTable  = [(Type, (FieldTable, MethodTable))]
type FunctionTable = [(QualifiedName, FunctionHeader)]
type ProgramTable = (ClassTable, FunctionTable)

buildProgramTable :: Program -> ProgramTable
buildProgramTable = buildClassTable &&& buildFunctionTable

buildClassTable :: Program -> ClassTable
buildClassTable p = map getClassEntry (classes p) ++
                    map getTraitEntry (traits p)
    where
    getClassEntry Class{cname, cfields, cmethods} =
      (cname, (map getFieldEntry cfields,
               map getMethodEntry cmethods))
    getTraitEntry Trait{tname, treqs, tmethods} =
        let (reqFields, reqMethods) = partition isRequiredField treqs
            fieldTable  = map (getFieldEntry . rfield) reqFields
            methodTable = map getReqMethodEntry reqMethods ++
                          map getMethodEntry tmethods
        in
          (tname, (fieldTable, methodTable))
    getFieldEntry f     = (fname f, f)
    getReqMethodEntry r = (hname . rheader $ r, rheader r)
    getMethodEntry m    = (methodName m, mheader m)

buildFunctionTable :: Program -> FunctionTable
buildFunctionTable p = map (fname &&& funheader) (functions p)
  where
    fname f@Function{funsource} =
      setSourceFile funsource . topLevelQName . functionName $ f

lookupClassEntry :: Type -> ClassTable -> (FieldTable, MethodTable)
lookupClassEntry ty ctable =
    let fail = error $ "ClassTable.hs: No entry for " ++ Types.showWithKind ty
    in snd $
       fromMaybe fail $ find ((== getId ty) . getId . fst) ctable

lookupField :: Type -> Name -> ProgramTable -> FieldDecl
lookupField ty f (ctable, _) =
    let (fs, _) = lookupClassEntry ty ctable
        fail = error $ "ClassTable.hs: No field '" ++ show f ++ "' in " ++
                       Types.showWithKind ty
    in fromMaybe fail $ lookup f fs

lookupMethod :: Type -> Name -> ProgramTable -> FunctionHeader
lookupMethod ty m (ctable, _) =
    let (_, ms) = lookupClassEntry ty ctable
        fail = error $ "ClassTable.hs: No method '" ++ show m ++ "' in " ++
                       Types.showWithKind ty
    in fromMaybe fail $ lookup m ms

lookupMethods :: Type -> ProgramTable -> [FunctionHeader]
lookupMethods cls (ctable, _) =
    let (_, ms) = lookupClassEntry cls ctable
    in map snd ms

lookupFunction :: QualifiedName -> ProgramTable -> FunctionHeader
lookupFunction qname@QName{qnsource = Just source, qnlocal} (_, ftable) =
  let failure = error $ "ClassTable.hs: Function '" ++ show qname ++
                       "' does not exist"
      key = setSourceFile source $
            topLevelQName qnlocal
  in fromMaybe failure (lookup key ftable)

lookupCalledType :: Type -> Name -> ProgramTable -> Type
lookupCalledType ty m table@(ctable, _)
  | isRefAtomType ty = ty
  | isUnionType ty =
      let tyAsCap = foldr1 disjunctiveType (unionMembers ty)
      in lookupCalledType tyAsCap m table
  | isCapabilityType ty =
      let traits = typesFromCapability ty
          ttable = map (\t -> (t, snd $ lookupClassEntry t ctable)) traits
          results = map (second (lookup m)) ttable
          fail = error $ "ClassTable.hs: No method '" ++ show m ++ "' in " ++
                 Types.showWithKind ty
      in
        fst . fromMaybe fail $ find (isJust . snd) results

getGlobalFunctionNames :: ProgramTable -> [QualifiedName]
getGlobalFunctionNames (_, ftable) = map fst ftable
