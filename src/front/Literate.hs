module Literate(getTangle) where

import Data.String.Utils
import Data.List
import Data.Char

getTangle :: String -> String
getTangle = unlines . getTangleAsList False . lines

getTangleAsList :: Bool -> [String] -> [String]
getTangleAsList inCodeBlock []
    | inCodeBlock = error $ "*** Error while extracting tangled code ***\n" ++
                            "Reached end of file before code block was closed"
    | otherwise = []
getTangleAsList inCodeBlock (l:ls)
    | inCodeBlock = if isClosingDelimiter l
                    then "" : getTangleAsList False ls
                    else l : getTangleAsList True ls
    | isOpeningDelimiter l = "" : getTangleAsList True ls
    | otherwise = "" : getTangleAsList False ls
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
