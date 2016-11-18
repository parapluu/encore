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
import qualified Data.Map.Strict as Map
import SystemUtils
import Language.Haskell.TH -- for Template Haskell hackery
import Text.Printf
import qualified Text.PrettyPrint.Boxes as Box
import System.FilePath (splitPath, joinPath)

import Makefile
import Utils
import Literate
import Parser.Parser
import AST.AST
import AST.PrettyPrinter
import AST.Desugarer
import ModuleExpander
import Typechecker.Environment
import Typechecker.Prechecker
import Typechecker.Typechecker
import Optimizer.Optimizer
import CodeGen.Main
import CodeGen.ClassDecl
import CodeGen.Preprocessor
import CodeGen.Header
import CCode.PrettyCCode
import Identifiers


-- the following line of code resolves the standard path at compile time using Template Haskell
standardLibLocation = $(stringE . init =<< runIO (System.Environment.getEnv "ENCORE_MODULES" ))

preludePaths =
  [standardLibLocation ++ "/standard", standardLibLocation ++ "/prototype"]

data Option =
              Run
            | Optimise String
            | Profile
            | KeepCFiles
            | Debug
            | CustomFlags String
            | Output FilePath
            | Source FilePath
            | Imports [FilePath]
            | TypecheckOnly
            | Verbose
            | Literate
            | NoGC
            | Help
            | Undefined String
            | Malformed String
              deriving Eq

data OptionHolder =
              Arg (String -> Option)
            | NoArg Option

data OptionMapping = OptionMapping {
  holder :: OptionHolder,
  short  :: String,
  long   :: String,
  optArg :: String,
  desc   :: String
}

optionMappings =
  map makeMapping
      [
       (Arg (Imports . split ":"), "-I", "--import", "[dirs]",
       "colon separated list of directories in which to look for modules."),
       (Arg Output, "-o", "--out-file", "[file]",
        "Specify output file."),
       (Arg CustomFlags, "-F", "--custom-flags", "[flags]",
        "Supply custom flags for the C compiler. (Use quotation marks to supply more than one flag)"),
       (NoArg KeepCFiles, "-c", "--generate-c", "",
        "Outputs intermediate C fields in separate source tree."),
       (NoArg Debug, "-g", "--debug", "",
        "Inserts debugging symbols in executable. Use with -c for improved debugging experience."),
       (NoArg TypecheckOnly, "-tc", "--type-check", "",
        "Only type check program, do not produce an executable."),
       (NoArg Literate, "", "--literate", "",
        "Literate programming mode. Code blocks are delimited by '#+begin_src' and '#+end_src'."),
       (NoArg Verbose, "-v", "--verbose", "",
        "Print debug information during compiler stages."),
       (Arg Optimise, "-O", "--optimize", "N",
        "Optimise produced executable. N=0,1,2 or 3."),
       (NoArg (Optimise "0"), "-O0", "", "", ""),
       (NoArg (Optimise "1"), "-O1", "", "", ""),
       (NoArg (Optimise "2"), "-O2", "", "", ""),
       (NoArg (Optimise "3"), "-O3", "", "", ""),
       (NoArg Profile, "-pg", "--profile", "",
        "Embed profiling information in the executable."),
       (NoArg Run, "", "--run", "",
        "Compile and run the program, but do not produce executable file."),
       (NoArg NoGC, "", "--no-gc", "",
        "DEBUG: disable GC and use C-malloc for allocation."),
       (NoArg Help, "", "--help", "",
        "Display this information.")
      ]
  where
    makeMapping (holder, short, long, optArg, desc) =
        OptionMapping{holder, short, long, optArg, desc}

findMapping arg =
  case find (\opt -> short opt == arg || long opt == arg) optionMappings of
    Just OptionMapping{holder} -> holder
    Nothing -> case arg of
                 '-':'-':flag -> NoArg $ Undefined flag
                 '-':flag -> NoArg $ Undefined flag
                 _ -> NoArg $ Source arg

-- args -> (programs, importDirs, options)
parseArguments :: [String] -> IO ([FilePath], [FilePath], [Option])
parseArguments args =
  let parseArguments' []   = []
      parseArguments' args = opt : parseArguments' rest
          where
            (opt, rest) = parseArgument args
            parseArgument (arg:args) =
              let holder = findMapping arg
              in case holder of
                   NoArg opt -> (opt, args)
                   Arg opt -> case args of
                                [] -> (Malformed arg, [])
                                (arg':rest) -> (opt arg', rest)
      (sources, rest) = partition isSource (parseArguments' args)
      (imports, options) = partition isImport rest
      importDirs homeDir =
        nub $ (preludePaths ++) $
          concatMap (\(Imports dirs) -> map getFullPath dirs) imports
          where
            getFullPath s =
              case splitPath s of
                ("~" : t) -> joinPath $ homeDir : t
                _ -> s
  in do
    homeDir <- getHomeDirectory
    return (map (\(Source name) -> name) sources,
            importDirs homeDir,
            options)
  where
    isSource (Source _) = True
    isSource _ = False
    isImport (Imports _) = True
    isImport _ = False

warnings :: [Option] -> IO ()
warnings options =
  when (NoGC `elem` options && TypecheckOnly `notElem` options)
       (putStrLn "Warning: Garbage collection disabled! Your program will leak memory!")

checkForUndefined :: [Option] -> IO ()
checkForUndefined =
  mapM_ (\flag -> case flag of
            Undefined flag ->
              abort $ "Unknown flag " <> flag <>
              ". Use --help to see available flags."
            Malformed flag ->
              abort $ "Not enough arguments to " <> flag <>
              ". Use --help to see correct usage."
            Optimise str ->
              unless (str `elem` ["0", "1", "2", "3"]) $
                abort $ "Illegal argument '" ++ str ++
                        "' to --optimise/-O. Use --help to see legal arguments."
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
           dropExt src = let src' = changeFileExt src ""
                         in if src == src'
                            then src'
                            else dropExt src'
           sourceName = dropExt sourcePath
           execName = case find isOutput options of
                        Just (Output file) -> file
                        Nothing            -> sourceName
           srcDir = sourceName ++ "_src"
       when (execName == sourcePath) $
            abort $ "Compilation would overwrite the source! Aborting.\n" ++
                    "You can specify the output file with -o [file]"
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
           customFlags = case find isCustomFlags options of
                           Just (CustomFlags str) -> str
                           Nothing                -> ""
           flags = "-std=gnu11 -Wall -fms-extensions -Wno-format -Wno-microsoft -Wno-parentheses-equality -Wno-unused-variable -Wno-unused-value" <+> customFlags <+> "-lpthread -ldl -lm -Wno-attributes"
           oFlag = "-o" <+> execName
           defines = getDefines options
           incs  = "-I" <+> incPath <+> "-I ."
           pg    = if Profile `elem` options then "-pg" else ""
           opt   = case find isOptimise options of
                        Just (Optimise str) -> "-O" ++ str
                        Nothing             -> ""
           debug = if Debug `elem` options then "-g" else ""
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

      isCustomFlags (CustomFlags _) = True
      isCustomFlags _ = False

      getDefines = unwords . map ("-D"++) .
                   filter (/= "") . map getDefine
      getDefine NoGC = "NO_GC"
      getDefine _ = ""

main =
    do args <- getArgs
       (programs, importDirs, options) <- parseArguments args
       checkForUndefined options
       when (Help `elem` options)
           (exit helpMessage)
       when (null programs)
           (abort ("No program specified! Aborting.\n\n" <>
                    usage <> "\n" <>
                    "The --help flag provides more information.\n"))
       warnings options
       let sourceName = head programs
       sourceExists <- doesFileExist sourceName
       unless sourceExists
           (abort $ "File \"" ++ sourceName ++ "\" does not exist! Aborting.")
       verbose options $ "== Reading file '" ++ sourceName ++ "' =="
       raw <- readFile sourceName
       code <- if Literate `elem` options || "#+literate\n" `isPrefixOf` raw
               then do
                 verbose options "== Extracting tangled code =="
                 return $ getTangle raw
               else
                   return raw

       verbose options "== Parsing =="
       ast <- case parseEncoreProgram sourceName code of
                Right ast  -> return ast
                Left error -> abort $ show error

       verbose options "== Importing modules =="
       programTable <- buildProgramTable importDirs preludePaths ast

       verbose options "== Desugaring =="
       let desugaredTable = fmap desugarProgram programTable

       verbose options "== Prechecking =="
       precheckedTable <- precheckProgramTable desugaredTable

       verbose options "== Typechecking =="
       typecheckedTable <- typecheckProgramTable precheckedTable

       verbose options "== Optimizing =="
       let optimizedTable = fmap optimizeProgram typecheckedTable

       verbose options "== Generating code =="
       let (mainDir, mainName) = dirAndName sourceName
           mainSource = mainDir </> mainName
       let fullAst = setProgramSource mainSource $
                     compressProgramTable optimizedTable

       unless (TypecheckOnly `elem` options) $
         case checkForMainClass mainSource fullAst of
           Just error -> abort $ show error
           Nothing    -> return ()

       exeName <- compileProgram fullAst sourceName options
       when (Run `elem` options)
           (do verbose options $ "== Running '" ++ exeName ++ "' =="
               system $ "./" ++ exeName
               system $ "rm " ++ exeName
               return ())
       verbose options "== Done =="

    where
      precheckProgramTable :: ProgramTable -> IO ProgramTable
      precheckProgramTable table = do
        let lookupTableTable = fmap buildLookupTable table
        mapM (precheckAndShowWarnings lookupTableTable) table
        where
          precheckAndShowWarnings table p = do
            (precheckedAST, precheckingWarnings) <-
                case precheckProgram table p of
                  (Right ast, warnings)  -> return (ast, warnings)
                  (Left error, warnings) -> do
                    showWarnings warnings
                    abort $ show error
            showWarnings precheckingWarnings
            return precheckedAST

      typecheckProgramTable :: ProgramTable -> IO ProgramTable
      typecheckProgramTable table = do
        let lookupTableTable = fmap buildLookupTable table
        mapM (typecheckAndShowWarnings lookupTableTable) table
        where
          typecheckAndShowWarnings table p = do
            (typecheckedAST, typecheckingWarnings) <-
                case typecheckProgram table p of
                  (Right (newEnv, ast), warnings) -> return (ast, warnings)
                  (Left error, warnings) -> do
                    showWarnings warnings
                    abort $ show error
            showWarnings typecheckingWarnings
            return typecheckedAST

      usage = "Usage: encorec [flags] file"
      verbose options str = when (Verbose `elem` options)
                                  (putStrLn str)

      showWarnings = mapM print
      helpMessage =
        "Welcome to the Encore compiler!\n" <>
        usage <> "\n\n" <>
        "Flags:\n" <>
        flags
        where
          mappingsWithDesc = filter (not . null . desc) optionMappings
          boxFlag f (opt@OptionMapping{optArg = ""}) = Box.text (f opt)
          boxFlag f (opt@OptionMapping{optArg}) = Box.text (f opt <+> optArg)
          longBox = Box.vcat Box.left $
                    map (boxFlag long) mappingsWithDesc
          shortBox = Box.vcat Box.left $
                     map (boxFlag (("|" <+>) . short)) mappingsWithDesc
          descBox = Box.vcat Box.left $
                    map (Box.text . desc) mappingsWithDesc
          optionBox = longBox Box.<+> shortBox Box.<+> descBox
          flags = intercalate "\n" $
                  map (("  " ++) . strip) . lines $
                  Box.render optionBox