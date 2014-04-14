
module Main where

import AST
import PrettyPrinter
import PonyBackend
-- import CodeGen

main = do
         putStrLn "Encore .... Off course."
         putStrLn $ show $ ppProgram example
         -- ccode = codeGenP example
  where example = AST.example
