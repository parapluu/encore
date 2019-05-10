module LSP.Data.DataMap(
  LSPData,
  DataMap,
  hasErrorInDataMap
  )
where

-- ###################################################################### --
-- Section: Imports
-- ###################################################################### --

-- Standard
import Data.Map as Map hiding (foldr)

-- LSP
import LSP.Data.Program
import LSP.Data.TextDocument

-- ###################################################################### --
-- Section: Data
-- ###################################################################### --

type LSPData = (Program, TextDocument)

type DataMap = Map String LSPData

-- ###################################################################### --
-- Section: Functions
-- ###################################################################### --

hasErrorInDataMap :: DataMap -> Bool
hasErrorInDataMap dataMap = Map.foldl (\acc (prog, _) -> acc || (length (errors prog)) > 0) False dataMap
