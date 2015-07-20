module CodeGen.Preprocessor(preprocess) where

import qualified AST.AST as A
import qualified AST.Util as Util
import qualified AST.Meta as Meta

preprocess :: A.Program -> A.Program
preprocess = inject_traits_to_classes . giveClosuresUniqueNames

inject_traits_to_classes :: A.Program -> A.Program
inject_traits_to_classes p@A.Program{A.classes} =
  p{A.classes = map inject_traits_to_class classes}
  where
    inject_traits_to_class :: A.ClassDecl -> A.ClassDecl
    inject_traits_to_class c@A.Class{A.cname, A.ctraits} =
      foldr inject_trait_to_class c $ map A.itrait ctraits

    inject_trait_to_class :: A.Trait -> A.ClassDecl -> A.ClassDecl
    inject_trait_to_class t@A.Trait{A.traitName} c@A.Class{A.methods} =
      let
        t_methods = A.traitMethods t
      in
        c{A.methods = methods ++ t_methods}

giveClosuresUniqueNames :: A.Program -> A.Program
giveClosuresUniqueNames ast = snd $ Util.extendAccumProgram uniqueClosureName 0 ast
    where
      uniqueClosureName acc e
          | A.isClosure e = let m = A.getMeta e
                            in (acc + 1, A.setMeta e (Meta.metaClosure ("closure" ++ show acc) m))
          | A.isTask e = let m = A.getMeta e
                         in (acc + 1, A.setMeta e (Meta.metaTask ("task" ++ show acc) m))
          | otherwise = (acc, e)
