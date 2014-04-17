
module Main where

import System.Environment

import AST
import PrettyPrinter
import Examples
import CodeGen
import PrettyCCode
import CodeGen

main = do
         putStrLn "// Encore .... Off course."
         args <- getArgs
         if null args then 
             putStrLn usage
         else
             do 
               program <- return (lookup (head args) examples)
               case program of 
                 Just ast -> do printCommented $ show $ ppProgram ast
                                printCommented "#####################"
                                print $ code_from_AST ast
                                -- ccode = codeGenP example             
                 Nothing -> do putStrLn "This is not a program that I can compile :("  
                               putStrLn "Available programs are:"  
                               printProgNames
    where
      usage = "Usage: ./encorec [program-name]"
      printCommented s = putStrLn $ unlines $ map ("//"++) $ lines s
      printProgNames = mapM_ putStrLn $ map fst examples