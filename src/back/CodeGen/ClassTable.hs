module CodeGen.ClassTable (
  ClassTable,
  FunctionTable,
  TableLookup(..),
  lookupMethod,
  lookupMethods,
  lookupField,
  lookupCalledType,
  lookupFunction,
  buildClassTable,
  buildFunctionTable) where

import Types
import AST.AST
import Identifiers

import Data.List
import Data.Maybe
import Control.Arrow

type FieldTable  = [(Name, FieldDecl)]
type MethodTable = [(Name, FunctionHeader)]
type FunctionTable = MethodTable
type ClassTable  = [(Type, (FieldTable, MethodTable))]
data TableLookup = CT { ct :: ClassTable }
                 | FT { ft :: FunctionTable }

buildClassTable :: Program -> ClassTable
buildClassTable = traverseProgram getEntries
  where
    getEntries p = map getClassEntry (classes p) ++
                   map getTraitEntry (traits p)
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
buildFunctionTable = traverseProgram getFunctions
  where
    getFunctions p = map getFunction (functions p)
    getFunction f = (functionName f, funheader f)

lookupEntry :: Type -> ClassTable -> (FieldTable, MethodTable)
lookupEntry ty ctable =
    let fail = error $ "ClassTable.hs: No entry for " ++ Types.showWithKind ty
    in snd $
       fromMaybe fail $ find ((== getId ty) . getId . fst) ctable

lookupField :: Type -> Name -> ClassTable -> FieldDecl
lookupField ty f ctable =
    let (fs, _) = lookupEntry ty ctable
        fail = error $ "ClassTable.hs: No field '" ++ show f ++ "' in " ++
                       Types.showWithKind ty
    in fromMaybe fail $ lookup f fs

lookupMethod :: Type -> Name -> ClassTable -> FunctionHeader
lookupMethod ty m ctable =
    let (_, ms) = lookupEntry ty ctable
        fail = error $ "ClassTable.hs: No method '" ++ show m ++ "' in " ++
                       Types.showWithKind ty
    in fromMaybe fail $ lookup m ms

lookupMethods :: Type -> ClassTable -> [FunctionHeader]
lookupMethods cls ctable =
    let (_, ms) = lookupEntry cls ctable
    in map snd ms

lookupCalledType :: Type -> Name -> ClassTable -> Type
lookupCalledType ty m ctable
  | isRefType ty = ty
  | isCapabilityType ty =
      let traits = typesFromCapability ty
          ttable = map (\t -> (t, snd $ lookupEntry t ctable)) traits
          results = map (second (lookup m)) ttable
          fail = error $ "ClassTable.hs: No method '" ++ show m ++ "' in " ++
                 Types.showWithKind ty
      in
        fst . fromMaybe fail $ find (isJust . snd) results

lookupFunction :: Type -> Name -> FunctionTable -> FunctionHeader
lookupFunction ty f ftable =
  let fail = error $ "ClassTable.hs: No function '" ++ show f ++ "' in " ++
             Types.showWithKind ty
    in fromMaybe fail $ lookup f ftable
