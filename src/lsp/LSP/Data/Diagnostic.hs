{-# LANGUAGE OverloadedStrings #-}

module LSP.Data.Diagnostic (
    PublishDiagnosticsParams(..),
    Diagnostic(..),
    DiagnosticSeverity(..),
    errorToDiagnostic
) where

import Data.Aeson

import LSP.Data.Position
import LSP.Data.Error

data PublishDiagnosticsParams = PublishDiagnosticsParams {
    pdpUri :: String,
    pdpDiagnostics :: [Diagnostic]
} deriving (Show)

data Diagnostic = Diagnostic {
    dRange :: Range,
    dSeverity :: DiagnosticSeverity,
    dCode :: String,
    dSource :: String,
    dMessage :: String
} deriving (Show)

data DiagnosticSeverity
    = DError
    | DWarning
    | DInformation
    | DHint
    deriving (Show, Enum)

errorToDiagnostic :: Error -> Diagnostic
errorToDiagnostic error =
    Diagnostic {
        dRange    = position error,
        dCode     = "-- error --",
        dSource   = "Encore",
        dMessage  = message error,
        dSeverity = case errorType error of
            TError   -> DError
            TWarning -> DWarning
            THint    -> DHint
    }

{-instance Enum DiagnosticSeverity where
    toEnum = fromIntegral . (-1)
    fromEnum = (+1) . fromInteger-}

instance ToJSON PublishDiagnosticsParams where
    toJSON pdp =
        object [
                "uri"         .= pdpUri pdp,
                "diagnostics" .= pdpDiagnostics pdp
            ]

instance ToJSON Diagnostic where
    toJSON diagnostic =
        object [
                "range"    .= object [
                        "start" .= object [
                                "line"      .= (fst $ fst $ dRange diagnostic),
                                "character" .= (snd $ fst $ dRange diagnostic)
                            ],
                        "end"   .= object [
                                "line"      .= (fst $ snd $ dRange diagnostic),
                                "character" .= (snd $ snd $ dRange diagnostic)
                            ]
                    ],
                "severity" .= (1 + (fromEnum $ dSeverity diagnostic)),
                "code"     .= dCode diagnostic,
                "source"   .= dSource diagnostic,
                "message"  .= dMessage diagnostic
            ]