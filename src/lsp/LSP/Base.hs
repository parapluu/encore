{- |
MODULE      : LSP.Base
DESCRIPTION : Implements the HTTP-like base layer of LSP
-}

module LSP.Base (
    encodeMessage,
    parsePackets, parsePacket,
    LSPPacket (..), LSPHeaderField (..)
) where

import Control.Monad
import Control.Concurrent
import Data.Aeson
import Data.Maybe
import Network.Socket
import qualified Network.Socket.ByteString.Lazy as SocketLazy (getContents)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Text.Read (readMaybe)

data LSPPacket = LSPPacket {
    header :: [LSPHeaderField],
    content :: String
} deriving (Show)

data LSPHeaderField = LSPHeaderField {
    key :: String,
    value :: String
} deriving (Show)

encodeMessage :: LSPPacket -> String
encodeMessage (LSPPacket header content) =
    headerString (headerWithContentLength) ++ content
    where
    headerWithContentLength =
        (LSPHeaderField "Content-Length" $ show $ length content):
        filter (\field -> key field /= "Content-Length") header
    headerString [] = "\r\n"
    headerString ((LSPHeaderField key value):bs) = key ++ (':':' ':value) ++ "\r\n" ++ headerString bs

{- |
parsePackets
 Parses the input into a list of packets.

Returns
the packets along with a possible error
that occurred while reading a packet.
-}
parsePackets :: String -> ([LSPPacket], Maybe String)
parsePackets input =
    case parsePacket input of
        Left e                -> ([], Just e)
        Right (request, rest) -> let (requests, e) = parsePackets rest
                                 in  (request:requests, e)

{- |
parsePacket
 Reads a single request.

Returns
an error description
or a packet along with any remaining text in the input string
after the end of the packet.
-}
parsePacket :: String -> Either String (LSPPacket, String)
parsePacket input =
    case readHeader input of
        Left error            -> Left error
        Right (fields, input) ->
            case map (readMaybe . value) $ filter ((== "Content-Length") . key) fields of
                [Just length] -> Right (LSPPacket fields $ take length input, drop length input)
                otherwise     -> Left "Missing Content-Length"

readHeader :: String -> Either String ([LSPHeaderField], String)
readHeader input =
    readHeader' input True
    where
    readHeader' input first =
        case (readHeaderField input, first) of
            (Left error, _)                -> Left error
            (Right (Nothing, rest), True)  -> readHeader' rest True
            (Right (Nothing, rest), False) -> Right ([], rest)
            (Right (Just field, rest), _)  ->
                 case readHeader' rest False of
                     Left error           -> Left error
                     Right (fields, rest) -> Right (field:fields, rest)


readHeaderField :: String -> Either String (Maybe LSPHeaderField, String)
readHeaderField input =
    case readLine input of
        Nothing           -> Left "End of input"
        Just ("",   rest) -> Right (Nothing, rest)
        Just (line, rest) ->
            case parseHeaderField line of
                Nothing           -> Left "Invalid header field"
                Just (key, value) -> Right (Just $ LSPHeaderField key value, rest)
    where
        parseHeaderField :: String -> Maybe (String, String)
        parseHeaderField ""              = Nothing
        parseHeaderField (':':' ':value) = Just ("", value)
        parseHeaderField (a:bs)          =
            case parseHeaderField bs of
                Nothing           -> Nothing
                Just (key, value) -> Just (a:key, value)

{- |
readLine
 Reads a single line of text terminated by
 "\r\n", "\n" or the end of the input.

Returns
Nothing or a pair containing
the line of text and any remaining text
in the input string after the end of the line.
-}
readLine :: String -> Maybe (String, String)
readLine ""               = Nothing
readLine ('\r':'\n':rest) = Just ("", rest)
readLine ('\n':rest)      = Just ("", rest)
readLine (a:rest)         =
    case readLine rest of
        Nothing           -> Just ([a], rest)
        Just (line, rest) -> Just (a:line, rest)
