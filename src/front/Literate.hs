module Literate(tangle) where

import Data.String.Utils
import Data.List
import Data.Char

tangle :: String -> String
tangle = unlines . tangleList False . lines

tangleList :: Bool -> [String] -> [String]
tangleList inCodeBlock []
    | inCodeBlock = error $ "*** Error while extracting tangled code ***\n" ++
                            "Reached end of file before code block was closed"
    | otherwise = []
tangleList inCodeBlock (l:ls)
    | inCodeBlock = if isClosingDelimiter l
                    then "" : tangleList False ls
                    else l : tangleList True ls
    | isOpeningDelimiter l = "" : tangleList True ls
    | otherwise = "" : tangleList False ls
      -- Filtered lines are replaced by an empty string to give
      -- error messages the correct line numbersm

isOpeningDelimiter :: String -> Bool
isOpeningDelimiter s =
    let delim = map toUpper $ strip s
        suffix = strip $ drop (length "#+BEGIN_SRC") delim
    in "#+BEGIN_SRC" `isPrefixOf` delim &&
       (null suffix || -- Default to encore code
        head suffix == '-' || head suffix == '+' || -- Allow flags
        "ENCORE" `isPrefixOf` suffix -- Allow specifying encore
       )

isClosingDelimiter :: String -> Bool
isClosingDelimiter s =
    let delim = map toUpper $ strip s
        suffix = strip $ drop (length "#+END_SRC") delim
    in "#+END_SRC" `isPrefixOf` delim &&
       (null suffix || "ENCORE" `isPrefixOf` suffix)
