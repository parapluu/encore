
module Main where

import AST
import PrettyPrinter
import Examples
import CodeGen
import PrettyCCode
-- import CodeGen

main = do
         putStrLn "Encore .... Off course."
         putStrLn $ show $ ppProgram hello
         putStrLn "##############"
         print $ code_from_AST hello
         -- ccode = codeGenP example
  where hello = Examples.hello
