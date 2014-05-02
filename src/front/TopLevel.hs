module Main where

import System.Environment
import System.IO
import System.Exit
import System.Process
import Data.List
import Control.Monad

import Parser
import AST
import PrettyPrinter
import Examples
import CodeGen.Main
import CCode.PrettyCCode

-- TODO: Add a -o option for output file
data Argument = GCC | Clang | KeepCFiles | Undefined | 
                File String deriving(Eq)

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

errorCheck :: [Argument] -> IO ()
errorCheck options = 
    do
      when (Undefined `elem` options) (putStrLn "Ignoring undefined options")
      when (GCC `elem` options) (putStrLn "Compilation with gcc not yet supported")
      when (Clang `elem` options && GCC `elem` options) (putStrLn "Conflicting compiler options. Defaulting to clang.")

outputCode :: Program -> Handle -> IO ()
outputCode ast out = 
    do printCommented "Source program: "
       printCommented $ show $ ppProgram ast
       printCommented "#####################"
       hPrint out $ code_from_AST ast
    where
      printCommented s = hPutStrLn out $ unlines $ map ("//"++) $ lines s

doCompile ast progName options = 
    do encorecPath <- getExecutablePath
       encorecDir <- return $ take (length encorecPath - length "encorec") encorecPath
       ponyLibPath <- return $ encorecDir ++ "../runtime/bin/debug/libpony.a"
       ponyRuntimeIncPath <- return $ encorecDir ++ "../runtime/inc/"
       execName <- return ("encore." ++ progName)
       cFile <- return (progName ++ ".pony.c")

       withFile cFile WriteMode (outputCode ast)
       when (Clang `elem` options) 
           (do putStrLn "Compiling with clang..." 
               exitCode <- system ("clang" <+> cFile <+> "-ggdb -o" <+> execName <+> ponyLibPath <+> "-I" <+> ponyRuntimeIncPath)
               case exitCode of
                 ExitSuccess -> putStrLn $ "Done! Output written to" <+> execName
                 ExitFailure n -> putStrLn $ "Compilation failed with exit code" <+> (show n))
       when ((Clang `elem` options) && not (KeepCFiles `elem` options))
           (do runCommand $ "rm -f" <+> cFile
               putStrLn "Cleaning up...")

(<+>) :: String -> String -> String
a <+> b = (a ++ " " ++ b)

main = 
    do
      putStrLn "Encore .... Off course."
      args <- getArgs
      if null args then 
          putStrLn usage
      else
          do
            (programs, options) <- return $ parseArguments args
            errorCheck options
            if null programs then
                do 
                  putStrLn "No program specified!"
                  putStrLn "Available programs are:"  
                  printProgNames
            else
                do
                  progName <- return (head programs)
                  code <- readFile progName
                  program <- return $ parseEncoreProgram progName code
                  case program of
                    Right ast -> doCompile ast progName options
                    Left error -> do putStrLn $ show error
    where
      usage = "Usage: ./encorec [-c|-gcc|-clang] [program-name]"
      printProgNames = mapM_ putStrLn $ map fst examples
