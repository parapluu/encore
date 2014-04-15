
module Main where

import AST
import PrettyPrinter
import Examples
import PonyBackend
-- import CodeGen

main = do
         putStrLn "Encore .... Off course."
         putStrLn $ show $ ppProgram hello
         -- ccode = codeGenP example
  where hello = Examples.hello
