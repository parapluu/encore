
module Main where

import AST
import PrettyPrinter
import CodeGen
import PrettyCCode
-- import CodeGen

main = do
         putStrLn "Encore .... Off course."
         putStrLn $ show $ ppProgram example
         putStrLn "##############"
         print $ code_from_AST example
         -- ccode = codeGenP example
