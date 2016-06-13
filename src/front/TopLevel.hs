{-# LANGUAGE NamedFieldPuns, TemplateHaskell #-}

{-|

The top level module that orchestrates the compilation process and
file I/O.

-}

module Main where

import System.Environment
import System.Directory
import System.IO
import System.Exit
import System.Process
import System.Posix.Directory
import Data.List
import Data.List.Utils(split)
import Data.Maybe
import Data.String.Utils
import Control.Monad
import qualified Data.Map as Map
import SystemUtils
import Language.Haskell.TH -- for Template Haskell hackery

import Makefile
import Utils
import Parser.Parser
import AST.AST
import AST.PrettyPrinter
import AST.Desugarer
import ModuleExpander
import Typechecker.Prechecker
import Typechecker.Typechecker
import Typechecker.Environment
import Optimizer.Optimizer
import CodeGen.Main
import CodeGen.ClassDecl
import CodeGen.Preprocessor
import CodeGen.Header
import CCode.PrettyCCode

import Identifiers


-- the following line of code resolves the standard path at compile time using Template Haskell
standardLibLocation = $(stringE . init =<< runIO (System.Environment.getEnv "ENCORE_BUNDLES" ))

data Phase = Parsed | TypeChecked
    deriving Eq

data Option = GCC | Clang | Run | Optimise String | Profile |
              KeepCFiles | Undefined String | Debug |
              Output FilePath | Source FilePath | Imports [FilePath] |
              Intermediate Phase | TypecheckOnly | Verbatim | NoGC | Help
              deriving Eq

parseArguments :: [String] -> ([FilePath], [FilePath], [Option])
parseArguments args =
    let
        parseArguments' []   = []
        parseArguments' args = opt : parseArguments' rest
            where
              (opt, rest) = parseArgument args
              parseArgument ("--generate-c":args)    = (KeepCFiles, args)
              parseArgument ("-c":args)              = (KeepCFiles, args)
              parseArgument ("--debug":args)         = (Debug, args)
              parseArgument ("-g":args)              = (Debug, args)
              parseArgument ("--type-check":args)    = (TypecheckOnly, args)
              parseArgument ("-tc":args)             = (TypecheckOnly, args)
              parseArgument ("--out-file":file:args) = (Output file, args)
              parseArgument ("-o":file:args)         = (Output file, args)
              parseArgument ("--optimise":level:args) = (Optimise level, args) 
              parseArgument ("-O":level:args)        = (Optimise level, args)
              parseArgument ("-O0":args)             = (Optimise "0", args)
              parseArgument ("-O1":args)             = (Optimise "1", args)
              parseArgument ("-O2":args)             = (Optimise "2", args)
              parseArgument ("-O3":args)             = (Optimise "3", args)
              parseArgument ("--run":args)           = (Run, args)
              parseArgument ("--no-gc":args)         = (NoGC, args)
              parseArgument ("--help":args)          = (Help, args)
              parseArgument ("--profile":args)       = (Profile, args)
              parseArgument ("-pg":args)             = (Profile, args)
              parseArgument ("--import":dirs:args)   = (Imports $ split ":" dirs, args)
              parseArgument ("-I":dirs:args)         = (Imports $ split ":" dirs, args)
              parseArgument ("--verbose":args)       = (Verbatim, args)
              parseArgument ("-v":args)              = (Verbatim, args)
              -- TODO: remove
              parseArgument ("-clang":args)          = (Clang, args)
              parseArgument ("-gcc":args)            = (GCC, args)
              parseArgument ("-AST":args)            = (Intermediate Parsed, args)
              parseArgument ("-TypedAST":args)       = (Intermediate TypeChecked, args)
              parseArgument (('-':flag):args)        = (Undefined flag, args)
              parseArgument (file:args)              = (Source file, args)
    in
      let (sources, aux) = partition isSource (parseArguments' args)
          (imports, options) = partition isImport aux
      in
      (map getName sources,
       ([standardLibLocation ++ "/standard/",
         standardLibLocation ++ "/prototype/",
         "./"] ++) $ map (++ "/") $ concatMap getDirs imports,
       options)
    where
      isSource (Source _) = True
      isSource _ = False
      getName (Source name) = name
      isImport (Imports _) = True
      isImport _ = False
      getDirs (Imports dirs) = dirs

warnings :: [Option] -> IO ()
warnings options =
    do
      when (GCC `elem` options && Clang `notElem` options)
               (putStrLn "Warning: Compilation with gcc not yet supported. Defaulting to clang")
      when (Clang `elem` options && GCC `elem` options)
               (putStrLn "Warning: Conflicting compiler options. Defaulting to clang.")
      when (TypecheckOnly `elem` options && (Clang `elem` options || GCC `elem` options))
               (putStrLn "Warning: Flag '-tc' specified. No executable will be produced")
      when (NoGC `elem` options && TypecheckOnly `notElem` options)
               (putStrLn "Warning: Garbage collection disabled! Your program will leak memory!")

checkForUndefined :: [Option] -> IO ()
checkForUndefined =
  mapM_ (\flag -> case flag of
            Undefined flag ->
              abort $ "Unknown flag " <> flag <>
              ". Use --help to see available flags."
            Optimise str ->
              if (str `elem` ["0", "1", "2", "3"]) then return () else
                abort $ "Illegal argument '" ++ str ++ "' to --optimise/-O. Use --help to see legal arguments."
            _ -> return ())

output :: Show a => a -> Handle -> IO ()
output ast = flip hPrint ast

writeClass srcDir (name, ast) =
    withFile (srcDir ++ "/" ++ name ++ ".encore.c") WriteMode (output ast)

processClassNames pairs =
  let (names, classes) = unzip pairs
      unprimed = map (replace "'" "_prime") names
      disambiguated = foldr disambiguate [] unprimed
      disambiguate name acc
        | name `elem` acc =
            let candidates = map ((name ++) . show) [0..]
                name' = fromJust $ find (`notElem` unprimed) candidates
            in name':acc
        | otherwise = name:acc
  in zip disambiguated classes

compileProgram prog sourcePath options =
    do encorecPath <- getExecutablePath
       let encorecDir = dirname encorecPath
           incPath = encorecDir <> "inc/"
           libPath = encorecDir <> "lib/"
           sourceName = changeFileExt sourcePath ""
           execName = case find isOutput options of
                        Just (Output file) -> file
                        Nothing            -> sourceName
           srcDir = sourceName ++ "_src"
       createDirectoryIfMissing True srcDir
       let emitted = compileToC prog
           classes = processClassNames (getClasses emitted)
           header = getHeader emitted
           shared = getShared emitted
       mapM_ (writeClass srcDir) classes
       let encoreNames =
             map (\(name, _) -> changeFileExt name "encore.c") classes
           classFiles = map (srcDir </>) encoreNames
           headerFile = srcDir </> "header.h"
           sharedFile = srcDir </> "shared.c"
           makefile   = srcDir </> "Makefile"
           cc    = "clang"
           flags = "-std=gnu11 -Wall -fms-extensions -Wno-format -Wno-microsoft -Wno-parentheses-equality -Wno-unused-variable -Wno-unused-value -lpthread -ldl -lm -Wno-attributes"
           oFlag = "-o" <+> execName
           defines = getDefines options
           incs  = "-I" <+> incPath <+> "-I ."
           pg    = if Profile `elem` options then "-pg" else ""
           opt   = case find isOptimise options of
                        Just (Optimise str) -> "-O" ++ str
                        Nothing             -> ""
           debug = if Debug `elem` options then (if GCC `elem` options then "-ggdb" else "-g") else ""
           libs  = libPath ++ "*.a"
           cmd   = pg <+> opt <+> flags <+> libs <+> incs <+> debug
           compileCmd = cc <+> cmd <+> oFlag <+> unwords classFiles <+>
                        sharedFile <+> libs <+> libs <+> defines
       withFile headerFile WriteMode (output header)
       withFile sharedFile WriteMode (output shared)
       withFile makefile   WriteMode (output $
          generateMakefile encoreNames execName cc cmd incPath defines libs)
       when ((TypecheckOnly `notElem` options) || (Run `elem` options))
           (do files  <- getDirectoryContents "."
               let ofilesInc = unwords (filter (isSuffixOf ".o") files)
               exitCode <- system $ compileCmd <+> ofilesInc
               case exitCode of
                 ExitSuccess -> return ()
                 ExitFailure n ->
                     abort $ " *** Compilation failed with exit code" <+> show n <+> "***")
       unless (KeepCFiles `elem` options)
                  (do runCommand $ "rm -rf" <+> srcDir
                      return ())
       return execName
    where
      isOutput (Output _) = True
      isOutput _ = False

      isOptimise (Optimise _) = True
      isOptimise _ = False

      getDefines = unwords . map ("-D"++) .
                   filter (/= "") . map getDefine
      getDefine NoGC = "NO_GC"
      getDefine _ = ""

main =
    do args <- getArgs
       let (programs, importDirs, options) = parseArguments args
       checkForUndefined options
       when (Help `elem` options)
           (abort helpMessage)
       when (null programs)
           (abort ("No program specified! Aborting.\n\n" <>
                    usage <> "\n" <>
                    "The --help flag provides more information.\n"))
       warnings options
       let sourceName = head programs
       sourceExists <- doesFileExist sourceName
       unless sourceExists
           (abort $ "File \"" ++ sourceName ++ "\" does not exist! Aborting.")
       verbatim options $ "== Reading file '" ++ sourceName ++ "' =="
       code <- readFile sourceName
       verbatim options "== Parsing =="
       ast <- case parseEncoreProgram sourceName code of
                Right ast  -> return ast
                Left error -> abort $ show error

       when (Intermediate Parsed `elem` options) $ do
         verbatim options "== Printing AST =="
         withFile (changeFileExt sourceName "AST") WriteMode
                  (flip hPrint $ show ast)

       verbatim options "== Expanding modules =="
       allModules <- importAndCheckModules (typecheck options sourceName) importDirs ast

       verbatim options "== Optimizing =="
       let optimizedModules = fmap optimizeProgram allModules

       verbatim options "== Generating code =="
       let fullAst = compressModules optimizedModules

       unless (TypecheckOnly `elem` options) $
         case checkForMainClass fullAst of
           Just error -> abort $ show error
           Nothing    -> return ()

       exeName <- compileProgram fullAst sourceName options
       when (Run `elem` options)
           (do verbatim options $ "== Running '" ++ exeName ++ "' =="
               system $ "./" ++ exeName
               system $ "rm " ++ exeName
               return ())
       verbatim options "== Done =="
    where
      typecheck :: [Option] -> FilePath -> Environment -> Program -> IO (Environment, Program)
      typecheck options sourceName env prog = do
         verbatim options "== Desugaring =="
         let desugaredAST = desugarProgram prog

         verbatim options "== Prechecking =="
         (precheckedAST, precheckingWarnings) <-
           case precheckEncoreProgram env desugaredAST of
             (Right ast, warnings)  -> return (ast, warnings)
             (Left error, warnings) -> do
               showWarnings warnings
               abort $ show error
         showWarnings precheckingWarnings

         verbatim options "== Typechecking =="
         ((newEnv, typecheckedAST), typecheckingWarnings) <-
           case typecheckEncoreProgram env precheckedAST of
             (Right (newEnv, ast), warnings) -> return ((newEnv, ast), warnings)
             (Left error, warnings) -> do
               showWarnings warnings
               abort $ show error
         showWarnings typecheckingWarnings

         when (Intermediate TypeChecked `elem` options) $ do
           verbatim options "== Printing typed AST =="
           withFile (changeFileExt sourceName "TAST") WriteMode
                    (flip hPrint $ show typecheckedAST)

         when (Intermediate TypeChecked `elem` options) $ do
           verbatim options "== Printing typed AST =="
           withFile (changeFileExt sourceName "TAST") WriteMode
                    (flip hPrint $ show typecheckedAST)

         return $ (newEnv, typecheckedAST)

      usage = "Usage: encorec [flags] file"
      verbatim options str = when (Verbatim `elem` options)
                                  (putStrLn str)

      showWarnings = mapM print
      helpMessage =
        "Welcome to the Encore compiler!\n" <>
        usage <> "\n\n" <>
        "Flags:\n" <>
        "  --import [dirs]   | -I [dirs] : separated list of directories in which to look for modules.\n" <>
        "  --out-file [file] | -o [file] Specify output file.\n" <>
        "  --generate-c      | -c        Outputs intermediate C files in separate source tree.\n" <>
        "  --debug           | -g        Inserts debugging symbols in executable. Use with -c for improved debugging experience.\n" <>
        "  --type-check      | -tc       Only type check program, do not produce an executable. \n" <>
        "  --verbose         | -v        Print debug information during compiler stages.\n" <>
        "  --optimise N      | -O N      Optimise produced executable. N=0,1,2 or 3. \n" <>
        "  --profile         | -pg       Embed profiling information in the executable.\n" <>
        "  --run                         Compile and run the program, but do not produce executable file.\n" <>
        "  --no-gc                       DEBUG: disable GC and use C-malloc for allocation.\n" <>
        "  --help                        Display this information.\n" <>
        "Obsolete flags (that will be removed):\n" <>
        "  -clang -gcc -AST -TypedAST" 

