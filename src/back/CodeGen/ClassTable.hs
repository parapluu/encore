module CodeGen.ClassTable (
  ClassTable,
  lookup_method,
  lookup_methods,
  lookup_field,
  build_class_table) where

import Types
import AST.AST
import Identifiers

import Data.List
import Data.Maybe

type FieldTable  = [(Name, FieldDecl)]
type MethodTable = [(Name, MethodDecl)]
type ClassTable  = [(Type, (FieldTable, MethodTable))]

build_class_table :: Program -> ClassTable
build_class_table = traverseProgram get_entries
  where
    get_entries p = map get_class_entry (classes p) ++
                    map get_trait_entry (traits p)
    get_class_entry Class{cname, cfields, cmethods} =
      (cname, (map get_field_entry cfields,
               map get_method_entry cmethods))
    get_trait_entry Trait{tname, tfields, tmethods} =
      (tname, (map get_field_entry tfields, map get_method_entry tmethods))
    get_field_entry f = (fname f, f)
    get_method_entry m = (mname m, m)

lookup_entry :: Type -> ClassTable -> (FieldTable, MethodTable)
lookup_entry ty ctable =
    let fail = error $ "ClassTable.hs: No entry for " ++ Types.showWithKind ty
    in snd $
       fromMaybe fail $ find ((== getId ty) . getId . fst) ctable

lookup_field :: Type -> Name -> ClassTable -> FieldDecl
lookup_field ty f ctable =
    let (fs, _) = lookup_entry ty ctable
        fail = error $ "ClassTable.hs: No field '" ++ show f ++ "' in " ++
                       Types.showWithKind ty
    in fromMaybe fail $ lookup f fs

lookup_method :: Type -> Name -> ClassTable -> MethodDecl
lookup_method ty m ctable =
    let (_, ms) = lookup_entry ty ctable
        fail = error $ "ClassTable.hs: No method '" ++ show m ++ "' in " ++
                       Types.showWithKind ty
    in fromMaybe fail $ lookup m ms

lookup_methods :: Type -> ClassTable -> [MethodDecl]
lookup_methods cls ctable =
    let (_, ms) = lookup_entry cls ctable
    in map snd ms
