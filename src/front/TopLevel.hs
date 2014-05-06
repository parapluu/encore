module Main where

import System.Exit
import System.Environment
import System.Directory
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

doCompile :: Program -> FilePath -> [Argument] -> IO ExitCode
doCompile ast sourceName options = 
    do encorecPath <- getExecutablePath
       encorecDir <- return $ take (length encorecPath - length "encorec") encorecPath
       ponyLibPath <- return $ encorecDir ++ "../runtime/bin/debug/libpony.a"
       ponyRuntimeIncPath <- return $ encorecDir ++ "../runtime/inc/"
       setPath <- return $ encorecDir ++ "../set/set.o"
       progName <- let ext = reverse . take 4 . reverse $ sourceName in 
                   if length sourceName > 3 && ext == ".enc" then 
                       return $ take ((length sourceName) - 4) sourceName 
                   else 
                       return sourceName
       execName <- return ("encore." ++ progName)
       cFile <- return (progName ++ ".pony.c")

       withFile cFile WriteMode (outputCode ast)
       if (Clang `elem` options) then
           do putStrLn "Compiling with clang..." 
              exitCode <- system ("clang" <+> cFile <+> "-ggdb -o" <+> execName <+> setPath <+> ponyLibPath <+> "-I" <+> ponyRuntimeIncPath)
              case exitCode of
                ExitSuccess -> putStrLn $ "Done! Output written to" <+> execName
                ExitFailure n -> putStrLn $ "Compilation failed with exit code" <+> (show n)
              when ((Clang `elem` options) && not (KeepCFiles `elem` options))
                       (do runCommand $ "rm -f" <+> cFile
                           putStrLn "Cleaning up...")
              return exitCode
       else
           return ExitSuccess

(<+>) :: String -> String -> String
a <+> b = (a ++ " " ++ b)

main = 
    do
      args <- getArgs
      if null args then 
          putStrLn usage
      else
          do
            (programs, options) <- return $ parseArguments args
            errorCheck options
            if null programs then
                putStrLn "No program specified! Aborting..."
            else
                do
                  progName <- return (head programs)
                  sourceExists <- doesFileExist progName
                  if not sourceExists then
                      do putStrLn ("File \"" ++ progName ++ "\" does not exist! Aborting..." )
                         exitFailure
                  else
                      do
                        code <- readFile progName
                        program <- return $ parseEncoreProgram progName code
                        case program of
                          Right ast -> do exitCode <- doCompile ast progName options
                                          exitWith exitCode
                          Left error -> do putStrLn $ show error
                                           exitFailure
    where
      usage = "Usage: ./encorec [-c | -gcc | -clang] file"
