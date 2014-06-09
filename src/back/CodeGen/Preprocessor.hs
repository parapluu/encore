module CodeGen.Preprocessor(preprocess) where

import qualified AST.AST as A
import qualified AST.Util as Util

preprocess :: A.Program -> A.Program
preprocess = giveClosuresUniqueNames

giveClosuresUniqueNames ast = snd $ Util.extendAccumProgram uniqueClosureName 0 ast
    where
      uniqueClosureName acc e
          | A.isClosure e = (acc + 1, A.setMetaId ("closure" ++ show acc) e)
          | otherwise = (acc, e)