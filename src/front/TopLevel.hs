module Main where

import System.Environment
import System.IO
import Data.List

import AST
import PrettyPrinter
import Examples
import CodeGen
import PrettyCCode
--import CodeGen

data Argument = GCC | Clang | KeepCFiles | Undefined | File String deriving(Eq)

parseArgument :: String -> Argument
parseArgument "-c" = KeepCFiles
parseArgument "-gcc" = GCC
parseArgument "-clang" = Clang
parseArgument ('-':_) = Undefined
parseArgument filename = File filename

parseArguments :: [String] -> ([String], [Argument])
parseArguments args = 
    let (files, options) = partition isFile (map parseArgument args) in
    (map getName files, options)
    where
      isFile (File _) = True
      isFile _ = False
      getName (File name) = name

outputCode :: Program -> Handle -> IO ()
outputCode ast out = 
    do printCommented $ show $ ppProgram ast
       printCommented "#####################"
       hPrint out $ code_from_AST ast
    where
      printCommented s = hPutStrLn out $ unlines $ map ("//"++) $ lines s

(<+>) :: String -> String -> String
a <+> b = (a ++ " " ++ b)

main = 
    do
      putStrLn "// Encore .... Off course."
      args <- getArgs
      if null args then 
          putStrLn usage
      else
          do
            (programs, options) <- return $ parseArguments args
            progName <- return (head programs)
            program <- return (lookup progName examples)
            case program of 
              Just ast -> do 
                           outfile <- return ("encore." ++ progName ++ ".pony.c")
                           withFile outfile WriteMode (outputCode ast)
                           -- Compile with clang and stuff...
              Nothing -> do putStrLn "This is not a program that I can compile :("  
                            putStrLn "Available programs are:"  
                            printProgNames
    where
      usage = "Usage: ./encorec [program-name]"
      printProgNames = mapM_ putStrLn $ map fst examples