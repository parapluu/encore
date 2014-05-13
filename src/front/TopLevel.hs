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
import Typechecker
import CodeGen.Main
import CCode.PrettyCCode

data Option = GCC | Clang | KeepCFiles | Typecheck | Undefined String | Output FilePath | Source FilePath deriving(Eq)

parseArguments :: [String] -> ([String], [Option])
parseArguments args = 
    let
        parseArguments' []   = []
        parseArguments' args = opt : (parseArguments' rest)
            where 
              (opt, rest) = parseArgument args
              parseArgument ("-c":args)       = (KeepCFiles, args)
              parseArgument ("-gcc":args)     = (GCC, args)
              parseArgument ("-clang":args)   = (Clang, args)
              parseArgument ("-t":args)       = (Typecheck, args)
              parseArgument ("-o":file:args)  = (Output file, args)
              parseArgument (('-':flag):args) = (Undefined flag, args)
              parseArgument (file:args)       = (Source file, args)
    in
      let (sources, options) = partition isSource (parseArguments' args) in
      (map getName sources, options)
    where
      isSource (Source _) = True
      isSource _ = False
      getName (Source name) = name

errorCheck :: [Option] -> IO ()
errorCheck options = 
    do
      mapM (\flag -> case flag of {Undefined flag -> putStrLn $ "Ignoring undefined option" <+> flag; _ -> return ()}) options
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

doCompile :: Program -> FilePath -> [Option] -> IO ExitCode
doCompile ast source options = 
    do encorecPath <- getExecutablePath
       encorecDir <- return $ take (length encorecPath - length "encorec") encorecPath
       incPath <- return $ encorecDir ++ "./inc/"
       ponyLibPath <- return $ encorecDir ++ "lib/libpony.a"
       setLibPath <- return $ encorecDir ++ "lib/set.o"

       

       progName <- return $ dropDir . dropExtension $ source
       execName <- case find (isOutput) options of
                     Just (Output file) -> return file
                     _                  -> return progName
       cFile <- return (progName ++ ".pony.c")

       withFile cFile WriteMode (outputCode ast)
       if (Clang `elem` options) then
           do putStrLn "Compiling with clang..." 
              exitCode <- system ("clang" <+> cFile <+> "-ggdb -o" <+> execName <+> ponyLibPath <+> setLibPath <+> "-I" <+> incPath)
              case exitCode of
                ExitSuccess -> putStrLn $ "Done! Output written to" <+> execName
                ExitFailure n -> putStrLn $ "Compilation failed with exit code" <+> (show n)
              when ((Clang `elem` options) && not (KeepCFiles `elem` options))
                       (do runCommand $ "rm -f" <+> cFile
                           putStrLn "Cleaning up...")
              return exitCode
       else
           return ExitSuccess

    where
      dropExtension source = let ext = reverse . take 4 . reverse $ source in 
                             if length source > 3 && ext == ".enc" then 
                                 take ((length source) - 4) source 
                             else 
                                 source
      dropDir = reverse . takeWhile (/='/') . reverse
      isOutput (Output _) = True
      isOutput _ = False

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
                          Right ast -> if not (Typecheck `elem` options) then
                                           do exitCode <- doCompile ast progName options
                                              exitWith exitCode
                                       else
                                           do tcResult <- return $ typecheckEncoreProgram ast
                                              case tcResult of
                                                Right () -> do exitCode <- doCompile ast progName options
                                                               exitWith exitCode
                                                Left err -> print err
                          Left error -> do putStrLn $ show error
                                           exitFailure
    where
      usage = "Usage: ./encorec [-c | -gcc | -clang] file"