module CodeGen.Preprocessor(preprocess) where

import qualified Data.List as L (lookup)
import Data.Maybe (fromJust)

import qualified AST.AST as A
import qualified AST.Util as Util
import qualified AST.Meta as Meta
import qualified Types as T

preprocess :: A.Program -> A.Program
preprocess = inject_traits_to_classes . giveClosuresUniqueNames

inject_traits_to_classes :: A.Program -> A.Program
inject_traits_to_classes p@A.Program{A.classes} =
  p{A.classes = map inject_traits_to_class classes}
  where
    inject_traits_to_class :: A.ClassDecl -> A.ClassDecl
    inject_traits_to_class c@A.Class{A.cname, A.ctraits} =
      foldr inject_trait_to_class c $ map (A.traitName . A.itrait) ctraits

    inject_trait_to_class :: T.Type -> A.ClassDecl -> A.ClassDecl
    inject_trait_to_class ty c@A.Class{A.methods} =
      let
        trait_template = A.getTrait ty p
        t_methods = get_trait_methods c ty trait_template
      in
        c{A.methods = methods ++ t_methods}

get_trait_methods :: A.ClassDecl -> T.Type -> A.Trait -> [A.MethodDecl]
get_trait_methods c ty template =
  let
    formals = T.getTypeParameters $ A.traitName template
    actuals = T.getTypeParameters ty
    this_binding = (ty, (A.getType c))
    bindings = this_binding : zip formals actuals
    converter old =
      case L.lookup old bindings of
        Just new -> new
        _ -> old
    methods = A.traitMethods template
  in
    map (substitute_types converter) methods
  where
    lookup x xs = fromJust $ L.lookup x xs

substitute_types :: (T.Type -> T.Type) -> A.MethodDecl -> A.MethodDecl
substitute_types converter method =
  let
    ret_type = A.mtype method
    ret_type' = T.typeMap converter ret_type
    params = A.mparams method
    params' = map convert_node params
    body = A.mbody method
    body' = Util.extend convert_node body
  in
    method{A.mtype = ret_type', A.mparams = params', A.mbody = body'}
  where
    convert_node :: A.HasMeta n => n -> n
    convert_node node =
      let
        old = A.getType node
        new = converter old
      in
        A.setType new node

giveClosuresUniqueNames :: A.Program -> A.Program
giveClosuresUniqueNames ast = snd $ Util.extendAccumProgram uniqueClosureName 0 ast
    where
      uniqueClosureName acc e
          | A.isClosure e = let m = A.getMeta e
                            in (acc + 1, A.setMeta e (Meta.metaClosure ("closure" ++ show acc) m))
          | A.isTask e = let m = A.getMeta e
                         in (acc + 1, A.setMeta e (Meta.metaTask ("task" ++ show acc) m))
          | otherwise = (acc, e)
