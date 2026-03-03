{-# LANGUAGE TemplateHaskell #-}

module LSP.Producer (
    produceAndUpdateState
) where

-- ###################################################################### --
-- Section: Imports
-- ###################################################################### --

-- Library
import Text.Megaparsec

-- Standard
import qualified Data.List.NonEmpty as NE(head)
import Language.Haskell.TH
import System.Environment
import System.Exit
import qualified Data.Map.Strict as Map
import Control.Monad
import Data.List
import Debug.Trace as Debug

-- Encore
import Parser.Parser
import qualified AST.AST as AST
import AST.Desugarer
import AST.PrettyPrinter
import ModuleExpander
import Typechecker.Environment
import Typechecker.Prechecker(precheckProgram)
import Typechecker.Typechecker(typecheckProgram, checkForMainClass)
import Typechecker.Capturechecker(capturecheckProgram)
import Typechecker.TypeError
import Utils

-- LSP
import LSP.Data.TextDocument
import LSP.Data.Error
import LSP.Data.DataMap
import LSP.Data.Program

-- ###################################################################### --
-- Section: Support
-- ###################################################################### --

-- the following line of code resolves the standard path at compile time using Template Haskell
standardLibLocation = $(stringE . init =<< runIO (System.Environment.getEnv "ENCORE_MODULES" ))

preludePaths =
    [standardLibLocation ++ "/standard", standardLibLocation ++ "/prototype"]

-- ###################################################################### --
-- Section: Data
-- ###################################################################### --

--type ProgramMap = Map FilePath Program

-- ###################################################################### --
-- Section: Functions
-- ###################################################################### --

produceAndUpdateState :: FilePath -> DataMap -> IO (DataMap)
produceAndUpdateState path dataMap = do
    case Map.lookup path dataMap of
      Nothing -> return (dataMap)
      Just (program, textDocument) -> do
        let source = tdContents textDocument
        -- Parse program to produce AST
        (_ast, error) <- case parseEncoreProgram path $ {- Debug.trace ("source: " ++ source) -} source of
          Right ast   -> return (ast, Nothing)
          Left error  -> return ((makeBlankAST path), Just error)

        case error of
          Just e -> do
            --print "Failed to parse program"
            let lspError = fromParsecError e
            newProgram <- case Map.lookup path dataMap of
              Just oldProgram -> do
                return Program {
                      ast = {- Debug.trace "found old program" $ -} ast (fst oldProgram),
                      errors = [lspError],
                      warnings = []
                      }

              Nothing -> do
                return Program {
                      ast = {- Debug.trace "found nothing" -} _ast,
                      errors = [lspError],
                      warnings = []
                      }

            let newDataMap = Map.insert path (newProgram, textDocument) dataMap
            return (newDataMap)
          Nothing -> do
            -- Build program table from AST
            programTable <- buildProgramTable preludePaths preludePaths $ {- Debug.trace "success" -} _ast
            let desugaredTable = fmap desugarProgram programTable

            -- Convert the desugared table into a LSPState
            let newDataMap = convertFromProgramTable path desugaredTable

            -- Precheck and typecheck the table
            precheckedTable <- producerPrecheck newDataMap
            newDataMap <- case hasErrorInDataMap precheckedTable of
              True  -> return precheckedTable
              False -> do
                typecheckedTable <- producerTypecheck precheckedTable
                case hasErrorInDataMap typecheckedTable of
                  True  -> return typecheckedTable
                  False -> producerCapturecheck typecheckedTable

            let cleanedMap = cleanDataMap newDataMap
            return (magicMerger dataMap cleanedMap)

cleanDataMap :: DataMap -> DataMap
cleanDataMap dataMap =
  Map.mapKeys cleanKey dataMap
  where
    cleanKey :: String -> String
    cleanKey key
      | isPrefixOf "./" key = drop 2 key
      | otherwise = key

magicMerger :: DataMap -> DataMap -> DataMap
magicMerger old new =
  Map.unionWith magicAux old new
  where
    magicAux :: LSPData -> LSPData -> LSPData
    magicAux _old _new =
      case (length (errors (fst _new))) > 0 of
        False -> (fst _new, snd _old)
        True  -> (Program {
                     ast = ast (fst _old),
                     errors = errors (fst _new),
                     warnings = warnings (fst _new)
                     },
                   snd _old)

convertFromProgramTable :: FilePath -> ProgramTable -> DataMap
convertFromProgramTable path table =
    fmap (convertFromProgram path) table

convertFromProgram :: FilePath -> AST.Program -> LSPData
convertFromProgram path _ast =
  (Program {ast = _ast, errors = [], warnings = []}, makeBlankTextDocument path)

convertToProgramTable :: DataMap -> ProgramTable
convertToProgramTable map =
    fmap convertToProgram map

convertToProgram :: LSPData -> AST.Program
convertToProgram (_program, textDocument) = ast _program

-- ###################################################################### --
-- Section: Type checking
-- ###################################################################### --

producerPrecheckProgram :: (Map.Map FilePath LookupTable) -> LSPData -> IO (LSPData)
producerPrecheckProgram lookupTable lspData@(oldProgram, textDocument) = do
    case precheckProgram lookupTable (convertToProgram lspData) of
        (Right newProgram, newWarnings) ->
          return (Program {
              ast = newProgram,
              errors = [],
              warnings = fromTCWarnings newWarnings
          }, textDocument)
        (Left error, newWarnings)->
          return (Program {
              ast = ast oldProgram,
              errors = [fromTCError error],
              warnings = fromTCWarnings newWarnings
          }, textDocument)

producerPrecheck :: DataMap -> IO (DataMap)
producerPrecheck programTable = do
    let lookupTable = fmap buildLookupTable (convertToProgramTable programTable)
    mapM (_producerPrecheck lookupTable) programTable
    where
        _producerPrecheck lookupTable program = do
            (precheckedProgram) <- (producerPrecheckProgram lookupTable program)
            return (precheckedProgram)

producerTypecheckProgram :: (Map.Map FilePath LookupTable) -> LSPData -> IO (LSPData)
producerTypecheckProgram lookupTable lspData@(oldProgram, textDocument) = do
    case typecheckProgram lookupTable (convertToProgram lspData) of
        (Right (_, newProgram), newWarnings) ->
          return (Program {
              ast = newProgram,
              errors = [],
              warnings = fromTCWarnings newWarnings
          }, textDocument)
        (Left error, newWarnings)->
          return (Program {
              ast = ast oldProgram,
              errors = [fromTCError error],
              warnings = fromTCWarnings newWarnings
          }, textDocument)

producerTypecheck :: DataMap -> IO (DataMap)
producerTypecheck programTable = do
    let lookupTable = fmap buildLookupTable (convertToProgramTable programTable)
    mapM (_producerTypecheck lookupTable) programTable
    where
        _producerTypecheck lookupTable program = do
            (typecheckedProgram) <- (producerTypecheckProgram lookupTable program)
            return (typecheckedProgram)

producerCapturecheckProgram :: (Map.Map FilePath LookupTable) -> LSPData -> IO (LSPData)
producerCapturecheckProgram lookupTable lspData@(oldProgram, textDocument) = do
    case capturecheckProgram lookupTable (convertToProgram lspData) of
        (Right (_, newProgram), newWarnings) ->
          return (Program {
              ast = newProgram,
              errors = [],
              warnings = fromTCWarnings newWarnings
          }, textDocument)
        (Left error, newWarnings)->
          return (Program {
              ast = ast oldProgram,
              errors = [fromTCError error],
              warnings = fromTCWarnings newWarnings
          }, textDocument)

producerCapturecheck :: DataMap -> IO (DataMap)
producerCapturecheck programTable = do
    let lookupTable = fmap buildLookupTable (convertToProgramTable programTable)
    mapM (_producerCapturecheck lookupTable) programTable
    where
        _producerCapturecheck lookupTable program = do
            (capturecheckedProgram) <- (producerCapturecheckProgram lookupTable program)
            return (capturecheckedProgram)
