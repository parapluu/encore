{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

{- |
Module : LSP.JSONRPC
-}

module LSP.JSONRPC (
    ClientMessage(..),
    ServerMessage(..),
    Error(..),

    parseMessage, encodeMessage,

    parseError, invalidRequest, methodNotFound, invalidParams, internalError
) where

import Data.Aeson hiding (Error)
import qualified Data.ByteString.Lazy.Char8 as BS
import GHC.Generics

data ClientMessage
    = Request {
        crMsgID  :: Value,
        crMethod :: String,
        crParams :: Maybe Value
    }
    | ClientNotification {
        cnMethod :: String,
        cnParams :: Maybe Value
    } deriving (Generic, Show)

data ServerMessage
    = Response {
        srMsgID  :: Value,
        srResult :: Value
    }
    | ErrorResponse {
        seMsgID :: Maybe Value,
        seError :: Error
    }
    | ServerNotification {
        snMethod :: String,
        snParams :: Maybe Value
    } deriving (Generic, Show)

data Error
    = Error {
        code    :: Integer,
        message :: String,
        errData :: Maybe Value
    }
    deriving (Generic, Show)

parseMessage :: String -> Either String [ClientMessage]
parseMessage input =
    case (eitherDecode $ BS.pack input) :: Either String ClientMessage of
        Right msg -> Right [msg]
        Left  e ->
            case (eitherDecode $ BS.pack input) :: Either String [ClientMessage] of
                Right []   -> Left e
                Left _     -> Left e
                Right msgs -> Right  msgs

encodeMessage :: ServerMessage -> String
encodeMessage = BS.unpack . encode

parseError     = -32700
invalidRequest = -32600
methodNotFound = -32601
invalidParams  = -32602
internalError  = -32603

instance FromJSON ClientMessage where
    parseJSON = withObject "Request/Notification" $ \o -> do
        jsonrpc <- o .:  "jsonrpc"
        msgID   <- o .:? "id"
        method  <- o .:  "method"
        params  <- o .:? "params"
        if jsonrpc /= "2.0"
        then fail $ "field \"jsonrpc\" must be \"2.0\", got \"" ++ jsonrpc ++ "\""
        else case msgID of
                 Just msgID -> return $ Request msgID method params
                 Nothing    -> return $ ClientNotification method params

instance ToJSON ServerMessage where
    toJSON (Response msgID result) =
        object $ [
            "jsonrpc" .= ("2.0" :: String),
            "id"      .= msgID,
            "result"  .= result]
    toJSON (ErrorResponse msgID error) =
        object $ [
            "jsonrpc" .= ("2.0" :: String),
            "id"      .= msgID,
            "error"   .= error]
    toJSON (ServerNotification method params) =
        object $ [
            "jsonrpc" .= ("2.0" :: String),
            "method"  .= method,
            "params"  .= params]

instance ToJSON Error where
    toJSON (Error code message errData) =
        object $ [
            "jsonrpc" .= ("2.0" :: String),
            "code"    .= code,
            "message" .= message,
            "data"    .= errData ]
