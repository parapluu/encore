module CodeGen.ClassTable (
  ClassTable,
  lookup_method,
  lookup_field,
  build_class_table) where

import Types
import AST.AST
import Identifiers

type FieldTable  = [(Name, FieldDecl)]
type MethodTable = [(Name, MethodDecl)]
type ClassTable  = [(Type, (FieldTable, MethodTable))]

build_class_table :: Program -> ClassTable
build_class_table = traverseProgram f merge
  where
    f Program{classes} = map get_class_entry classes
    merge a b = a ++ concat b
    get_class_entry Class{cname, fields, methods} =
      (cname, ((map get_field_entry fields), (map get_method_entry methods)))
    get_field_entry f@Field{fname} = (fname, f)
    get_method_entry m = (mname m, m)


lookup_field :: ClassTable -> Type -> Name -> Maybe FieldDecl
lookup_field ctable cls f =
    do (fs, _) <- lookup cls ctable
       lookup f fs


lookup_method :: ClassTable -> Type -> Name -> Maybe MethodDecl
lookup_method ctable cls m =
    do (_, ms) <- lookup cls ctable
       lookup m ms
