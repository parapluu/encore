module LSP.Data.Error (
  Error(..),
  ErrorType(..),
  fromTCError,
  fromTCErrors,
  fromTCWarning,
  fromTCWarnings,
  fromParsecError,
  fromErrorMessage
) where

-- ###################################################################### --
-- Section: Imports
-- ###################################################################### --

-- Library
import Text.Megaparsec

-- Standard
import qualified Data.Map.Strict as Map
import qualified Data.List.NonEmpty as NE(head)
import Data.List
import Control.Monad

-- Encore imports
import Typechecker.TypeError hiding (Error)
import AST.AST
import qualified AST.Meta as ASTMeta(Position(SingletonPos, RangePos))

-- ###################################################################### --
-- Section: Data
-- ###################################################################### --

{- Position tuple  -}
type Position = ((Int, Int), (Int, Int))

data ErrorType = TError | TWarning | THint

instance Show ErrorType where
  show (TError) = "Error"
  show (TWarning) = "Warning"
  show (THint) = "Hint"

{- Error datastructure. Can represent a variety of different error types -}
data Error = Error{
    message     :: String,      -- Error message string
    position    :: Position,    -- Start and end position
    errorType   :: ErrorType
} deriving (Show)

-- ###################################################################### --
-- Section: Functions
-- ###################################################################### --

extractTCErrorPosition :: TCError -> Position
extractTCErrorPosition error@(TCError errorType []) = ((0, 0), (0, 0))
extractTCErrorPosition error@(TCError errorType backtrace) =
    case fst (head backtrace) of
        (ASTMeta.SingletonPos startPos) ->
            ((getPosLine startPos - 1, getPosCol startPos - 1),
            (getPosLine startPos - 1, getPosCol startPos - 1))
        (ASTMeta.RangePos startPos endPos) ->
            ((getPosLine startPos - 1, getPosCol startPos - 1),
            (getPosLine endPos - 1, getPosCol endPos - 1))
    where getPosLine = fromIntegral . unPos . sourceLine
          getPosCol  = fromIntegral . unPos . sourceColumn

extractTCWarningPosition :: TCWarning -> Position
extractTCWarningPosition warning = ((0,0), (0,0))

fromTCError :: TCError -> Error
fromTCError error =
    Error{message = show error, position = extractTCErrorPosition error, errorType = TError}

fromTCErrors :: [TCError] -> [Error]
fromTCErrors errors = fmap (fromTCError) errors

fromTCWarning :: TCWarning -> Error
fromTCWarning warning =
    Error{message = show warning, position = extractTCWarningPosition warning, errorType = TWarning}

fromTCWarnings :: [TCWarning] -> [Error]
fromTCWarnings warnings = fmap (fromTCWarning) warnings

fromParsecError :: (ParseError Char Text.Megaparsec.Dec) -> Error
fromParsecError error =
    Error{
        message = show error,
        position = ((fromIntegral(unPos $ sourceLine $ NE.head $ errorPos error), fromIntegral(unPos $ sourceColumn $ NE.head $ errorPos error)),
                    (fromIntegral(unPos $ sourceLine $ NE.head $ errorPos error), fromIntegral(unPos $ sourceColumn $ NE.head $ errorPos error))),
        errorType = TError
    }

{- Construct a Error from a message, position and whether it is actually a
    warning

    Param: Error message
    Param: Position (row, column)
    Param: Whether or not the error is actually a warning
-}
fromErrorMessage :: String -> Position -> ErrorType -> Error
fromErrorMessage _message _position _errorType =
    Error{message = _message, position = _position, errorType = _errorType}
