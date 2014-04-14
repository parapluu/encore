
module Main where

import AST
import PrettyPrinter

main = do
         putStrLn "Encore .... Off course."
         putStrLn $ show $ ppProgram example
  where example = AST.example
