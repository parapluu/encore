{-|

The top level module that orchestrates the compilation process and
file I/O.

-}

module Main where

import System.Exit
import System.Environment
import System.Directory
import System.IO
import System.Exit
import System.Process
import System.Posix.Directory
import Data.List
import Control.Monad

import Parser.Parser
import AST.AST
import AST.PrettyPrinter
import AST.Util
import AST.Desugarer
import Typechecker.Typechecker
import Optimizer.Optimizer
import CodeGen.Main
import CodeGen.Preprocessor
import CCode.PrettyCCode

data Phase = Parsed | TypeChecked 
    deriving Eq

data Option = GCC | Clang | Run | KeepCFiles | Undefined String | Output FilePath | Source FilePath | Intermediate Phase
	deriving Eq

parseArguments :: [String] -> ([FilePath], [Option])
parseArguments args = 
    let
        parseArguments' []   = []
        parseArguments' args = opt : (parseArguments' rest)
            where 
              (opt, rest) = parseArgument args
              parseArgument ("-c":args)         = (KeepCFiles, args)
              parseArgument ("-gcc":args)       = (GCC, args)
              parseArgument ("-run":args)       = (Run, args)
              parseArgument ("-clang":args)     = (Clang, args)
              parseArgument ("-o":file:args)    = (Output file, args)
              parseArgument ("--AST":args)      = (Intermediate Parsed, args)
              parseArgument ("--TypedAST":args) = (Intermediate TypeChecked, args)
              parseArgument (('-':flag):args)   = (Undefined flag, args)
              parseArgument (file:args)         = (Source file, args)
    in
      let (sources, options) = partition isSource (parseArguments' args) in
      (map getName sources, options)
    where
      isSource (Source _) = True
      isSource _ = False
      getName (Source name) = name

warnUnknownFlags :: [Option] -> IO ()
warnUnknownFlags options = 
    do
      mapM (\flag -> case flag of {Undefined flag -> putStrLn $ "Ignoring undefined option" <+> flag; _ -> return ()}) options
      when (GCC `elem` options) (putStrLn "Compilation with gcc not yet supported")
      when (Clang `elem` options && GCC `elem` options) (putStrLn "Conflicting compiler options. Defaulting to clang.")

outputCode :: Program -> Handle -> IO ()
outputCode ast out = 
    do printCommented "Source program: "
       printCommented $ show $ ppProgram ast
       printCommented $ show ast
       printCommented "#####################"
       hPrint out $ code_from_AST ast
    where
      printCommented s = hPutStrLn out $ unlines $ map ("// "++) $ lines s

compileProgram :: Program -> FilePath -> [Option] -> IO ()
compileProgram prog exe_path options =
    do encorecPath <- getExecutablePath
       let encorecDir = take (length encorecPath - length "encorec") encorecPath
       let incPath = encorecDir ++ "./inc/"
       let libPath = encorecDir ++ "./lib/"
       execName <- case find (isOutput) options of
                     Just (Output file) -> return file
                     Nothing            -> return exe_path
       let cFile = exe_path ++ ".pony.c"
       withFile cFile WriteMode (outputCode prog)
       if ((Clang `elem` options) || (Run `elem` options)) then
           do
             files  <- getDirectoryContents "."
             let ofilesInc = concat $ intersperse " " (Data.List.filter (isSuffixOf ".o") files)
             let cmd = "clang" <+> 
                       cFile <+> 
                       ofilesInc <+> 
                       "-ggdb -Wall -Wno-unused-variable -lpthread" <+>
                       " -o" <+> execName <+>
                       (libPath++"*.a") <+>
                       (libPath++"*.a") <+>
                       "-I" <+> incPath <+> "-I ."
             exitCode <- system cmd
             case exitCode of
               ExitSuccess -> return ()
               ExitFailure n -> do
                             when (not (KeepCFiles `elem` options))
                                (do runCommand $ "rm -f" <+> cFile
                                    return ())
                             fail $ "Compilation failed with exit code" <+> (show n)
       else
           return ()

    where
      dropDir = reverse . takeWhile (/='/') . reverse
      isOutput (Output _) = True
      isOutput _ = False

(<+>) :: String -> String -> String
a <+> b = (a ++ " " ++ b)

-- withTemporaryEncFileName :: (FilePath -> IO ()) -> IO ()
-- withTemporaryEncFileName f = withFile "tmp.enc" WriteMode f

main = 
    do args <- getArgs
       let (programs, options) = parseArguments args
       warnUnknownFlags options
       when (null programs)
           (do putStrLn usage
               fail "No program specified! Aborting.")
       let sourceName = head programs
       let exeName = dropExtension sourceName
       sourceExists <- doesFileExist sourceName
       when (not sourceExists)
           (fail $ "File \"" ++ sourceName ++ "\" does not exist! Aborting.")
       code <- readFile sourceName
       ast <- case parseEncoreProgram sourceName code of
                Right ast  -> return ast
                Left error -> fail $ show error
       when (Intermediate Parsed `elem` options) 
           (withFile (exeName ++ ".AST") WriteMode 
               (flip hPrint $ show ast))
       let desugaredAST = desugarProgram ast
       typecheckedAST <- case typecheckEncoreProgram desugaredAST of
                           Right ast  -> return ast
                           Left error -> fail $ show error
       when (Intermediate TypeChecked `elem` options)
           (withFile (exeName ++ ".TAST") WriteMode 
               (flip hPrint $ show ast))
       let optimizedAST = optimizeProgram typecheckedAST
       compileProgram optimizedAST exeName options
       when (Run `elem` options) 
           (do system $ "./" ++ exeName
               system $ "rm " ++ exeName
               return ())
    where
      usage = "Usage: ./encorec [ -c | -gcc | -clang | -o file | -run | --AST | --TypedAST ] file"

-- some utility functions (TODO: move some to general utility library)
dropExtension source =
	let ext = lastn 4 source in 
	if length source > 3 && ext == ".enc" then 
		take ((length source) - 4) source 
	else 
		source
		
lastn n = reverse . take n . reverse
