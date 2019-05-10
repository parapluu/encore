{-# LANGUAGE OverloadedStrings #-}

module LSP.Data.TextDocument (
    TextDocument(..),
    TextDocumentClose(..),
    TextDocumentChange(..),
    TextDocumentIdent(..),
    uri,
    version,
    applyTextDocumentChange,
    makeBlankTextDocument
) where

-- ###################################################################### --
-- Section: Imports
-- ###################################################################### --

-- Standard
import Data.Aeson
import Data.String.Utils (split, join)

-- LSP
import LSP.Data.Position

-- ###################################################################### --
-- Section: Data
-- ###################################################################### --

data TextDocument = TextDocument {
    tdUri :: String,
    tdVersion :: Int,
    tdLanguageId :: String,
    tdContents :: String
} deriving (Show)

data TextDocumentClose = TextDocumentClose {
    tdclIdentifier :: TextDocumentIdent
} deriving (Show)

data TextDocumentChange = TextDocumentChange {
    tdcIdentifier :: TextDocumentIdentVersion,
    tdcChanges :: [TextDocumentContentChange]
} deriving (Show)

data TextDocumentIdent = TextDocumentIdent {
    tdiUri :: String
} deriving (Show)

data TextDocumentIdentVersion = TextDocumentIdentVersion {
    tdivUri :: String,
    tdivVersion :: Int
} deriving (Show)

data TextDocumentContentChange = TextDocumentContentChange {
    tdccRange :: Range,
    tdccRangeLength :: Int,
    tdccText :: String
} deriving (Show)

-- ###################################################################### --
-- Section: Functions
-- ###################################################################### --

applyTextDocumentChange :: TextDocumentChange -> TextDocument -> TextDocument
applyTextDocumentChange textDocumentChange textDocument =
    foldr (\change textDocument ->
            TextDocument {
                tdUri        = tdUri textDocument,
                tdVersion    = tdivVersion $ tdcIdentifier textDocumentChange,
                tdLanguageId = tdLanguageId textDocument,
                tdContents   = applyTextChange (tdccRange change)
                                               (tdccText change)
                                               (tdContents textDocument)
            }
        ) textDocument $ tdcChanges textDocumentChange

applyTextChange :: Range -> String -> String -> String
applyTextChange _ replacement "" = replacement
applyTextChange a@((startLine, startChar), (endLine, endChar))
                replacement
                text
    = let textLines = split "\n" text
          clamp = max 0 . min (length textLines - 1)
          boundedStartLine = clamp startLine
          boundedEndLine   = clamp endLine
          startSegment = join "\n" $ take startLine textLines ++
                                     [take startChar $ textLines !! boundedStartLine]
          endSegment = join "\n" $ (drop endChar $ textLines !! boundedEndLine) :
                                   drop (endLine+1) textLines
      in startSegment ++ replacement ++ endSegment

makeBlankTextDocument :: String -> TextDocument
makeBlankTextDocument name = TextDocument{
    tdUri      = name,
    tdVersion  = 1,
    tdLanguageId = "encore",
    tdContents   = ""
}

-- ###################################################################### --
-- Section: Type Classes
-- ###################################################################### --

class TextDocumentURI a where
    uri :: a -> String
class TextDocumentVersion a where
    version :: a -> Int

instance TextDocumentURI TextDocument where
    uri = tdUri
instance TextDocumentVersion TextDocument where
    version = tdVersion

instance TextDocumentURI TextDocumentChange where
    uri = uri . tdcIdentifier
instance TextDocumentVersion TextDocumentChange where
    version = version . tdcIdentifier

instance TextDocumentURI TextDocumentIdent where
    uri = tdiUri

instance TextDocumentURI TextDocumentIdentVersion where
    uri = tdivUri
instance TextDocumentVersion TextDocumentIdentVersion where
    version = tdivVersion

instance FromJSON TextDocument where
    parseJSON = withObject "params" $ \o -> do
        document <- o .: "textDocument"
        uri      <- document .: "uri"
        version  <- document .: "version"
        id       <- document .: "languageId"
        text     <- document .: "text"
        return TextDocument {
            tdUri = uri,
            tdVersion = version,
            tdLanguageId = id,
            tdContents = text
        }

instance FromJSON TextDocumentClose where
    parseJSON = withObject "params" $ \o -> do
        identifier <- o .: "textDocument"

        return TextDocumentClose {
            tdclIdentifier = identifier
        }

instance FromJSON TextDocumentChange where
    parseJSON = withObject "params" $ \o -> do
        identifier  <- o .: "textDocument"
        changes     <- o .: "contentChanges"

        return TextDocumentChange {
            tdcIdentifier = identifier,
            tdcChanges = changes
        }

instance FromJSON TextDocumentContentChange where
    parseJSON = withObject "TextDocumentChange" $ \o -> do
        range       <- o .: "range"
        rangeStart  <- range .: "start"
        startLine   <- rangeStart .: "line"
        startChar   <- rangeStart .: "character"
        rangeEnd    <- range .: "end"
        endLine     <- rangeEnd .: "line"
        endChar     <- rangeEnd .: "character"
        rangeLength <- o .: "rangeLength"
        text        <- o .: "text"

        return TextDocumentContentChange {
            tdccRange = ((startLine, startChar), (endLine, endChar)),
            tdccRangeLength = rangeLength,
            tdccText = text
        }

instance FromJSON TextDocumentIdent where
    parseJSON = withObject "textDocumentIdentifier" $ \o -> do
        uri <- o .: "uri"
        return TextDocumentIdent {
            tdiUri = uri
        }

instance FromJSON TextDocumentIdentVersion where
    parseJSON = withObject "textDocumentIdentifier" $ \o -> do
        uri <- o .: "uri"
        version <- o .: "version"
        return TextDocumentIdentVersion {
            tdivUri = uri,
            tdivVersion = version
        }
