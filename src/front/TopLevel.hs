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
import Optimizer.Optimizer
import CodeGen.Main
import CodeGen.ClassDecl
import CodeGen.Preprocessor
import CodeGen.Header
import CCode.PrettyCCode

import Text.Parsec.Pos as P
import qualified AST.Meta as Meta
import Identifiers

-- the following line of code resolves the standard path at compile time using Template Haskell
standardLibLocation = $(stringE . init =<< runIO (System.Environment.getEnv "ENCORE_BUNDLES" ))

data Phase = Parsed | TypeChecked
    deriving Eq

data Option = GCC | Clang | Run | Bench | Profile |
              KeepCFiles | Undefined String |
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
              parseArgument ("-bench":args)     = (Bench, args)
              parseArgument ("-pg":args)        = (Profile, args)
              parseArgument ("-c":args)         = (KeepCFiles, args)
              parseArgument ("-tc":args)        = (TypecheckOnly, args)
              parseArgument ("-gcc":args)       = (GCC, args)
              parseArgument ("-run":args)       = (Run, args)
              parseArgument ("-clang":args)     = (Clang, args)
              parseArgument ("-o":file:args)    = (Output file, args)
              parseArgument ("-AST":args)       = (Intermediate Parsed, args)
              parseArgument ("-TypedAST":args)  = (Intermediate TypeChecked, args)
              parseArgument ("-I":dirs:args)    = (Imports $ split ":" dirs, args)
              parseArgument ("-v":args)         = (Verbatim, args)
              parseArgument ("-nogc":args)      = (NoGC, args)
              parseArgument ("-help":args)      = (Help, args)
              parseArgument (('-':flag):args)   = (Undefined flag, args)
              parseArgument (file:args)         = (Source file, args)
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
              ". Use -help to see available flags."
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
           flags = "-std=gnu11 -ggdb -Wall -fms-extensions -Wno-format -Wno-microsoft -Wno-parentheses-equality -Wno-unused-variable -Wno-unused-value -lpthread -ldl -lm -Wno-attributes"
           oFlag = "-o" <+> execName
           defines = getDefines options
           incs  = "-I" <+> incPath <+> "-I ."
           pg    = if Profile `elem` options then "-pg" else ""
           bench = if Bench `elem` options then "-O3" else ""
           libs  = libPath ++ "*.a"
           cmd   = cc <+> pg <+> bench <+> flags <+> oFlag <+> libs <+> incs
           compileCmd = cmd <+> unwords classFiles <+>
                        sharedFile <+> libs <+> libs <+> defines
       withFile headerFile WriteMode (output header)
       withFile sharedFile WriteMode (output shared)
       withFile makefile   WriteMode (output $
          generateMakefile encoreNames execName cc flags incPath defines libs)
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

      getDefines = unwords . map ("-D"++) .
                   filter (/= "") . map getDefine
      getDefine NoGC = "NO_GC"
      getDefine _ = ""

main =
    do args <- getArgs
       let (programs, importDirs, options) = parseArguments args
       checkForUndefined options
       when (Help `elem`options)
           (abort helpMessage)
       when (null programs)
           (abort ("No program specified! Aborting.\n\n" <>
                    usage <> "\n" <>
                    "The -help flag provides more information.\n"))
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
       allModules <- tabulateImportedModules (typecheck options sourceName) importDirs (addStdLib ast) -- TODO: addStdLib should probably NOT happen here
       expandedAst <- expandModules importDirs (addStdLib ast) -- TODO: this should probably NOT happen here



       let optimizedAST = undefined -- need to get this from the hashtable
       return undefined -- fix, obviouslt
{-
       verbatim options "== Generating code =="
       exeName <- compileProgram optimizedAST sourceName options
       when (Run `elem` options)
           (do verbatim options $ "== Running '" ++ exeName ++ "' =="
               system $ "./" ++ exeName
               system $ "rm " ++ exeName
               return ())
       verbatim options "== Done =="
 -}
    where
      typecheck options sourceName table prog = do
         verbatim options "== Desugaring =="
         let desugaredAST = desugarProgram prog

         verbatim options "== Prechecking =="
         (precheckedAST, precheckingWarnings) <-
           case precheckEncoreProgram desugaredAST of
             (Right ast, warnings)  -> return (ast, warnings)
             (Left error, warnings) -> do
               showWarnings warnings
               abort $ show error
         showWarnings precheckingWarnings

         verbatim options "== Typechecking =="
         (typecheckedAST, typecheckingWarnings) <-
           case typecheckEncoreProgram precheckedAST of
             (Right ast, warnings)  -> return (ast, warnings)
             (Left error, warnings) -> do
               showWarnings warnings
               abort $ show error
         showWarnings typecheckingWarnings

         when (Intermediate TypeChecked `elem` options) $ do
           verbatim options "== Printing typed AST =="
           withFile (changeFileExt sourceName "TAST") WriteMode
                    (flip hPrint $ show typecheckedAST)

         verbatim options "== Optimizing =="
         return $ optimizeProgram typecheckedAST
        
      usage = "Usage: encorec [flags] file"
      verbatim options str = when (Verbatim `elem` options)
                                  (putStrLn str)
      addStdLib ast@Program{imports = i} = ast{imports = i ++ stdLib}
      -- TODO: move this elsewhere
      stdLib = [Import (Meta.meta (P.initialPos "String.enc")) [Name "String"]]

      showWarnings = mapM print
      helpMessage =
        "Welcome to the Encore compiler!\n" <>
        usage <> "\n\n" <>
        "Flags:\n" <>
        "  -c           Keep intermediate C-files.\n" <>
        "  -tc          Typecheck only (don't produce an executable).\n" <>
        "  -o [file]    Specify output file.\n" <>
        "  -run         Run the program and remove the executable.\n" <>
        "  -clang       Use clang to build the executable (default).\n" <>
        "  -AST         Output the parsed AST as text to foo.AST.\n" <>
        "  -TypedAST    Output the typechecked AST as text to foo.TAST.\n" <>
        "  -nogc        Disable the garbage collection of passive objects.\n" <>
        "  -help        Print this message and exit.\n" <>
        "  -I p1:p2:... Directories in which to look for modules."
