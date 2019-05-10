module LSP.Service  (
    startServer,
    ConnectionParams(..)
) where

-- ###################################################################### --
-- Section: Imports
-- ###################################################################### --

-- Haskell imports
import Control.Monad (forever)
import Control.Concurrent (forkIO)
import qualified Data.Map.Strict as Map
import Data.List
import Network
import System.IO

-- Encore imports
import ModuleExpander
import AST.AST

-- LSP imports
import LSP.LSP
import LSP.ServerTest

-- ###################################################################### --
-- Section: Data
-- ###################################################################### --

{-
-}
data ConnectionParams
    = STDIO
    | TCPServer Integer
    | TCPClient String Integer
    deriving (Show)

-- ###################################################################### --
-- Section: Functions
-- ###################################################################### --

{- Start the Encore compiler in LSP mode. This will handle events from
a client and handle them accordingly.
Param: ConnectionParams specifying mode and possibly host and port
-}
startServer :: ConnectionParams -> IO ()
startServer STDIO = do
    testServer
    return ()

    hSetBuffering stdout NoBuffering
    handleClient stdin stdout

startServer (TCPServer port) = do
    sock <- listenOn $ PortNumber $ fromInteger port
    putStrLn $ "listening on " ++ (show port)
    forever $ do
        (client, addr, _) <- accept sock
        putStrLn $ "connection from " ++ (show addr)

        forkIO $ do hSetBuffering client NoBuffering
                    handleClient client client

startServer (TCPClient host port) = do
    putStrLn $ "connecting to " ++ (show host) ++ " :" ++ (show port)

    sock <- connectTo host $ PortNumber $ fromInteger port

    hSetBuffering sock NoBuffering
    handleClient sock sock
