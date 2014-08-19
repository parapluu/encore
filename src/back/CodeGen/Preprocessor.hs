module CodeGen.Preprocessor(preprocess) where

import qualified AST.AST as A
import qualified AST.Util as Util
import qualified AST.Meta as Meta

preprocess :: A.Program -> A.Program
preprocess = giveClosuresUniqueNames

giveClosuresUniqueNames :: A.Program -> A.Program
giveClosuresUniqueNames ast = snd $ Util.extendAccumProgram uniqueClosureName 0 ast
    where
      uniqueClosureName acc e
          | A.isClosure e = let m = A.getMeta e
                            in (acc + 1, A.setMeta e (Meta.metaClosure ("closure" ++ show acc) m))
          | otherwise = (acc, e)