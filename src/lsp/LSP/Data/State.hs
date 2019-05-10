module LSP.Data.State (
    LSPState(..),
    initial,
    lookupTextDocument,
    addTextDocument,
    closeTextDocument,
    changeTextDocument,
    produceTextDocument,
    compileDocument,
    getProgram
) where

-- ###################################################################### --
-- Section: Imports
-- ###################################################################### --

-- Standard
import Data.Map as Map hiding (foldr)

-- LSP
import LSP.Data.TextDocument
import LSP.Data.Program
import LSP.Data.DataMap
import LSP.Producer

-- ###################################################################### --
-- Section: Data
-- ###################################################################### --

data LSPState = LSPState {
    programs :: DataMap
} deriving (Show)

-- ###################################################################### --
-- Section: Functions
-- ###################################################################### --

initial :: LSPState
initial = LSPState {
    programs = Map.empty
}

lookupTextDocument :: String -> LSPState -> Maybe (TextDocument)
lookupTextDocument uri state =
  case Map.lookup uri (programs state) of
    Just res -> Just $ snd res
    Nothing -> Nothing

addTextDocument :: TextDocument -> LSPState -> LSPState
addTextDocument newDocument (LSPState programs) =
    LSPState (Map.insert (uri newDocument) (makeBlankProgram (uri newDocument), newDocument) programs)

closeTextDocument :: TextDocumentIdent -> LSPState -> LSPState
closeTextDocument ident = LSPState . Map.delete (uri ident) . programs

changeTextDocument :: TextDocumentChange -> LSPState -> LSPState
changeTextDocument documentChange state@(LSPState programs) =
    case Map.lookup (uri documentChange) programs of
        Nothing -> state
        Just (program, textDocument) ->
            LSPState $ Map.insert (uri documentChange)
                                  (program, applyTextDocumentChange documentChange textDocument)
                                  programs

produceTextDocument :: TextDocument -> LSPState -> IO (LSPState)
produceTextDocument textDocument state = do
  let path = uri textDocument
  dataMap <- produceAndUpdateState path (programs state)
  return (LSPState {programs = dataMap})

compileDocument :: String -> LSPState -> IO LSPState
compileDocument path = fmap LSPState . produceAndUpdateState path . programs

getProgram :: String -> LSPState -> Maybe Program
getProgram path = fmap fst . Map.lookup path . programs
