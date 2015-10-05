module CodeGen.ClassTable (
  ClassTable,
  lookupMethod,
  lookupMethods,
  lookupField,
  buildClassTable) where

import Types
import AST.AST
import Identifiers

import Data.List
import Data.Maybe

type FieldTable  = [(Name, FieldDecl)]
type MethodTable = [(Name, MethodDecl)]
type ClassTable  = [(Type, (FieldTable, MethodTable))]

buildClassTable :: Program -> ClassTable
buildClassTable = traverseProgram getEntries
  where
    getEntries p = map getClassEntry (classes p) ++
                   map getTraitEntry (traits p)
    getClassEntry Class{cname, cfields, cmethods} =
      (cname, (map getFieldEntry cfields,
               map getMethodEntry cmethods))
    getTraitEntry Trait{tname, tfields, tmethods} =
      (tname, (map getFieldEntry tfields, map getMethodEntry tmethods))
    getFieldEntry f = (fname f, f)
    getMethodEntry m = (mname m, m)

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

lookupMethod :: Type -> Name -> ClassTable -> MethodDecl
lookupMethod ty m ctable =
    let (_, ms) = lookupEntry ty ctable
        fail = error $ "ClassTable.hs: No method '" ++ show m ++ "' in " ++
                       Types.showWithKind ty
    in fromMaybe fail $ lookup m ms

lookupMethods :: Type -> ClassTable -> [MethodDecl]
lookupMethods cls ctable =
    let (_, ms) = lookupEntry cls ctable
    in map snd ms
