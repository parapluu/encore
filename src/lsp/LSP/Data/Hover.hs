{-# LANGUAGE OverloadedStrings #-}

module LSP.Data.Hover where

-- ###################################################################### --
-- Section: Imports
-- ###################################################################### --
import LSP.Data.Position
import Data.Aeson

-- ###################################################################### --
-- Section: Data
-- ###################################################################### --
data TextDocumentPositionParams = TextDocumentPositionParams {
    uri :: String,
    position :: Position
}

data Hover = Hover {
    contents :: String,
    range :: Range
}

-- ###################################################################### --
-- Section: Type Classes
-- ###################################################################### --

instance FromJSON TextDocumentPositionParams where
    parseJSON = withObject "params" $ \o -> do
        textDocument    <- o .: "textDocument"
        uri             <- textDocument .: "uri"
        position        <- o .: "position"
        line            <- position .: "line"
        character       <- position .: "character"

        return TextDocumentPositionParams {
            uri = uri,
            position = (line + 1, character + 1)
        }

instance ToJSON Hover where
    toJSON hover =
        object [
            "contents" .= contents hover,
            "range"    .= object [
                "start" .= object [
                    "line"      .= ((fst $ fst $ range hover) - 1),
                    "character" .= ((snd $ fst $ range hover) - 1)
                ],
                "end"   .= object [
                    "line"      .= ((fst $ snd $ range hover) - 1),
                    "character" .= ((snd $ snd $ range hover) - 1)
                ]
            ]
        ]
