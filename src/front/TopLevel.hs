{-# LANGUAGE NamedFieldPuns #-}

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
import Data.List.Utils(split)
import Control.Monad
import SystemUtils

import Utils
import Parser.Parser
import AST.AST
import AST.PrettyPrinter
import AST.Util
import AST.Desugarer
import ModuleExpander
import Typechecker.Typechecker
import Optimizer.Optimizer
import CodeGen.Main
import CodeGen.ClassDecl
import CodeGen.Preprocessor
import CodeGen.Header
import CCode.PrettyCCode

data Phase = Parsed | TypeChecked 
    deriving Eq

data Option = GCC | Clang | Run | 
              KeepCFiles | Undefined String | 
              Output FilePath | Source FilePath | Imports [FilePath] |
              Intermediate Phase | TypecheckOnly
	deriving Eq

parseArguments :: [String] -> ([FilePath], [FilePath], [Option])
parseArguments args = 
    let
        parseArguments' []   = []
        parseArguments' args = opt : (parseArguments' rest)
            where 
              (opt, rest) = parseArgument args
              parseArgument ("-c":args)         = (KeepCFiles, args)
              parseArgument ("-tc":args)        = (TypecheckOnly, args)
              parseArgument ("-gcc":args)       = (GCC, args)
              parseArgument ("-run":args)       = (Run, args)
              parseArgument ("-clang":args)     = (Clang, args)
              parseArgument ("-o":file:args)    = (Output file, args)
              parseArgument ("-AST":args)       = (Intermediate Parsed, args)
              parseArgument ("-TypedAST":args)  = (Intermediate TypeChecked, args)
              parseArgument ("-I":dirs:args)    = (Imports $ split "," dirs, args) -- HERE HERE HERE
              parseArgument (('-':flag):args)   = (Undefined flag, args)
              parseArgument (file:args)         = (Source file, args)
    in
      let (sources, aux) = partition isSource (parseArguments' args) 
          (imports, options) = partition isImport aux
      in
      (map getName sources, ("./" :) $ map (++ "/") $ concat $ map getDirs imports, options)
    where
      isSource (Source _) = True
      isSource _ = False
      getName (Source name) = name
      isImport (Imports _) = True
      isImport _ = False
      getDirs (Imports dirs) = dirs

warnUnknownFlags :: [Option] -> IO ()
warnUnknownFlags options = 
    do
      mapM (\flag -> case flag of 
                       Undefined flag -> putStrLn $ "Warning: Ignoring undefined option" <+> flag
                       _ -> return ()) options
      when (GCC `elem` options && (not $ Clang `elem` options)) 
               (putStrLn "Warning: Compilation with gcc not yet supported. Defaulting to clang")
      when (Clang `elem` options && GCC `elem` options) 
               (putStrLn "Warning: Conflicting compiler options. Defaulting to clang.")
      when ((TypecheckOnly `elem` options) && (Clang `elem` options || GCC `elem` options)) 
               (putStrLn "Warning: Flag '-tc' specified. No executable will be produced")


outputCode ast out = hPrint out ast    

writeClass srcDir (name, ast) = withFile (srcDir ++ "/" ++ name ++ ".pony.c") WriteMode (outputCode ast)

compileProgram prog sourcePath options =
    do encorecPath <- getExecutablePath
       let encorecDir = dirname encorecPath
           incPath = encorecDir </> "inc/"
           libPath = encorecDir </> "lib/"
           sourceName = changeFileExt sourcePath ""
           execName = case find (isOutput) options of
                        Just (Output file) -> file
                        Nothing            -> sourceName
           srcDir = (sourceName ++ "_src")
       createDirectoryIfMissing True srcDir
       let (classes, header, shared) = compile_to_c prog
       mapM (writeClass srcDir) classes
       let classFiles = map (\(name, _) -> (srcDir </> changeFileExt name "pony.c")) classes
           headerFile = srcDir </> "header.h"
           sharedFile = srcDir </> "shared.c"
       withFile headerFile WriteMode (outputCode header)
       withFile sharedFile WriteMode (outputCode shared)
       when ((not $ TypecheckOnly `elem` options) || (Run `elem` options))
           (do files  <- getDirectoryContents "."
               let ofilesInc = concat $ intersperse " " (Data.List.filter (isSuffixOf ".o") files)
                   cmd = "clang" <+> 
                         concat (intersperse " " classFiles) <+> 
                         sharedFile <+>
                         ofilesInc <+> 
                         "-ggdb -Wall -Wno-unused-variable -lpthread" <+>
                         " -o" <+> execName <+>
                         (libPath++"*.a") <+>
                         (libPath++"*.a") <+>
                         "-I" <+> incPath <+> "-I ."
               exitCode <- system cmd
               case exitCode of
                 ExitSuccess -> return ()
                 ExitFailure n -> 
                     abort $ " *** Compilation failed with exit code" <+> (show n) <+> "***")
       unless (KeepCFiles `elem` options)
                  (do runCommand $ "rm -rf" <+> srcDir
                      return ())
       return execName
    where
      isOutput (Output _) = True
      isOutput _ = False

main = 
    do args <- getArgs
       let (programs, importDirs, options) = parseArguments args
       warnUnknownFlags options
       when (null programs)
           (do putStrLn usage
               abort "No program specified! Aborting.")
       let sourceName = head programs
       sourceExists <- doesFileExist sourceName
       when (not sourceExists)
           (abort $ "File \"" ++ sourceName ++ "\" does not exist! Aborting.")
       code <- readFile sourceName
       ast <- case parseEncoreProgram sourceName code of
                Right ast  -> return ast
                Left error -> abort $ show error
       when (Intermediate Parsed `elem` options) 
           (withFile (changeFileExt sourceName "AST") WriteMode 
               (flip hPrint $ show ast))
       expandedAst <- expandModules importDirs ast
       let desugaredAST = desugarProgram expandedAst
       typecheckedAST <- case typecheckEncoreProgram desugaredAST of
                           Right ast  -> return ast
                           Left error -> abort $ show error
       when (Intermediate TypeChecked `elem` options)
           (withFile (changeFileExt sourceName "TAST") WriteMode 
               (flip hPrint $ show ast))
       let optimizedAST = optimizeProgram typecheckedAST
       exeName <- compileProgram optimizedAST sourceName options
       when (Run `elem` options) 
           (do system $ "./" ++ exeName
               system $ "rm " ++ exeName
               return ())
    where
      usage = "Usage: ./encorec [ -c | -gcc | -clang | -o file | -run | -AST | -TypedAST ] file"
