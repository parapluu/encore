{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LSP.LSP (
    handleClient
) where

-- ###################################################################### --
-- Section: Imports
-- ###################################################################### --

-- Standard
import Data.Aeson as Aeson
import Data.Aeson.Types
import Data.Text (pack)
import System.IO
import Control.Monad.State
import Debug.Trace as Debug

-- LSP
import qualified LSP.Base as Base
import LSP.JSONRPC as JSONRPC
import LSP.Data.State as State
import LSP.Data.TextDocument as TextDocument
import LSP.Data.Hover (Hover(..))
import qualified LSP.Data.Hover as Hover
import LSP.Data.Diagnostic as Diagnostic
import LSP.Data.Program as Program
import LSP.Producer (produceAndUpdateState)

-- ###################################################################### --
-- Section: Functions
-- ###################################################################### --

decodeMessageStream :: String -> ([Either String JSONRPC.ClientMessage], Maybe String)
decodeMessageStream input =
    let (message, e)   = Base.parsePackets input
        requestBatches = map (JSONRPC.parseMessage . Base.content) message
        requests       = concat $ map (either ((:[]) .Left) (map Right)) requestBatches
    in  (requests, e)

encodeMessageStream :: [JSONRPC.ServerMessage] -> String
encodeMessageStream responses =
    let responseMessages = map (Base.LSPPacket [] . JSONRPC.encodeMessage) responses
    in  concat $ map Base.encodeMessage responseMessages

handleClient :: Handle -> Handle -> IO ()
handleClient input output =
    do inputStream <- hGetContents input
       let (requests, e) = decodeMessageStream inputStream
       sendResponses output State.initial $ map handleRequest requests
       return ()

sendResponses :: Handle -> LSPState -> [StateT LSPState IO [ServerMessage]] -> IO ()
sendResponses _ _ [] = return ()
sendResponses output state (a:bs) =
    do (responses, nextState) <- runStateT a state
       hPutStr output $ encodeMessageStream responses
       sendResponses output nextState bs

modifyM :: (Monad m, MonadTrans t, MonadState s (t m)) => (s -> m s) -> t m ()
modifyM f = do s <- get
               s <- lift $ f s
               put s

{- Uses the regular handleRequest but prints the request and response messages as well. -}
debugHandleRequest :: Either String JSONRPC.ClientMessage ->
                      StateT LSPState IO [ServerMessage]
debugHandleRequest request =
    do lift $ putStrLn $ "got request: " ++ show request
       state <- get
       (responses, newState) <- lift $ runStateT (handleRequest request) state
       put newState
       lift $ putStrLn $ "sending responses: " ++ show responses
       return responses

handleRequest :: Either String JSONRPC.ClientMessage ->
                 StateT LSPState IO [ServerMessage]
handleRequest (Left e)
    = return [
              JSONRPC.ErrorResponse {
                  seMsgID = Nothing,
                  seError = JSONRPC.Error JSONRPC.parseError e Nothing
              }
          ]

handleRequest (Right (Request msgID "initialize" params))
    = (lift $ putStrLn "init") >> return [
              ServerNotification {
                  snMethod = "window/showMessage",
                  snParams = Just $ object [
                          ("type", Number 3), -- info
                          ("message", "LSP - Hello world!")
                      ]
              },
              Response {
                  srMsgID  = msgID,
                  srResult = object [
                      ("capabilities", object [
                          ("textDocumentSync", Number 2), -- Incremental
                          ("hoverProvider", Bool True)
                      ])
                  ]
              }
          ]

handleRequest (Right (ClientNotification "textDocument/didOpen" params))
    = case fmap fromJSON params of
          Just (Success document) ->
              do modify $ State.addTextDocument document
                 modifyM $ State.compileDocument (uri document)

                 program <- fmap (getProgram $ uri document) get
                 case program of
                     Just p ->
                         return [
                                 ServerNotification {
                                         snMethod = "textDocument/publishDiagnostics",
                                         snParams = Just $ toJSON $ PublishDiagnosticsParams {
                                                 pdpUri = uri document,
                                                 pdpDiagnostics = map errorToDiagnostic $ errors p
                                             }
                                     }
                             ]
                     Nothing -> return []
          Just (Aeson.Error err) -> return []
          Nothing -> return []

handleRequest (Right (ClientNotification "textDocument/didClose" params))
    = case fmap fromJSON params of
          Just (Success documentIdent) ->
              do lift $ putStrLn $ "close " ++ (show documentIdent)
                 modify $ State.closeTextDocument (tdclIdentifier documentIdent)
                 return []
          Just (Aeson.Error err) -> return []
          Nothing -> return []

handleRequest (Right (ClientNotification "textDocument/didChange" params))
    = case fmap fromJSON params of
          Just (Success documentChange) ->
              do modify $ State.changeTextDocument documentChange
                 modifyM $ State.compileDocument (uri documentChange)

                 program <- fmap (getProgram $ uri documentChange) get
                 case program of
                     Just p ->
                         return [
                                 ServerNotification {
                                         snMethod = "textDocument/publishDiagnostics",
                                         snParams = Just $ toJSON $ PublishDiagnosticsParams {
                                                 pdpUri = uri documentChange,
                                                 pdpDiagnostics = map errorToDiagnostic $ errors p
                                             }
                                     }
                             ]
                     Nothing -> return []
          Just (Aeson.Error err) ->
              do lift $ putStrLn $ show err
                 return []
          Nothing -> return []

handleRequest (Right (Request msgID "textDocument/hover" params))
    = case fmap fromJSON params of
          Just (Success posParams) ->
              do state <- get
                 case State.getProgram (Hover.uri posParams) state >>=
                          getProgramInfoForPos ({- Debug.trace ("hover pos: " ++ show (Hover.position posParams)) -} (Hover.position posParams)) of
                     Nothing ->
                        return [
                                Response {
                                    srMsgID = {- Debug.trace "hover nothing: " -} msgID,
                                    srResult = Null
                                }
                            ]
                     Just info ->
                         return [
                                Response {
                                    srMsgID = {- Debug.trace "hover just: " -} msgID,
                                    srResult = toJSON $ Hover {
                                        Hover.contents = pDesc info,
                                        Hover.range = pRange info
                                    }
                                }
                            ]
          Just (Aeson.Error err) -> return []
          Nothing -> return []

{- ignored notifications -}
handleRequest (Right (ClientNotification "initialized" _)) = return []
handleRequest (Right (ClientNotification "$/cancelRequest" _)) = return []
handleRequest (Right (ClientNotification "textDocument/didSave" _)) = return []
handleRequest (Right (ClientNotification "workspace/didChangeConfiguration" _)) = return []
handleRequest (Right (ClientNotification method params))
    = return [
              showMessage MessageInfo $ "Unknown notification from client: " ++ (show method)
          ]

handleRequest (Right (Request msgID method params))
    = return [
              showMessage MessageError $ "Unknown request from client: " ++ (show method),
              ErrorResponse {
                  seMsgID = Just msgID,
                  seError = JSONRPC.Error JSONRPC.methodNotFound "method not found" Nothing
              }
          ]



data ServerMessageLevel = MessageError |
                          MessageWarning |
                          MessageInfo |
                          MessageLog
                          deriving (Enum)

showMessage :: ServerMessageLevel -> String -> ServerMessage
showMessage level msg =
    JSONRPC.ServerNotification {
        snMethod = "window/showMessage",
        snParams = Just $ object [
                "type"    .= (1 + fromEnum level),
                "message" .= msg
            ]
    }
