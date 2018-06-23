{-# LANGUAGE OverloadedStrings #-}

module Typechecker.ExplainTable (
     Table
    ,lookupHash
    ,getErrorExplanation
    ) where

import Text.PrettyPrint
import Text.Read (readMaybe)


lookupHash :: String -> Maybe Int
lookupHash k = 
    let T t = table in
        lookup' t k 0
  where
    lookup' [] k _ = Nothing
    lookup' ((k', v):as) k x
        |  k == k'= Just x
        | otherwise = lookup' as k (x+1)


getErrorExplanation :: String -> Maybe Doc
getErrorExplanation ('E':err) =
    case readMaybe err :: Maybe Int of
        Just num
            | num > 0 -> let T t = table in lookupExplain num t
            | otherwise -> Nothing
        Nothing -> Nothing
getErrorExplanation _ = Nothing


lookupExplain _ [] = Nothing
lookupExplain 0 ((_, v):_) = Just v
lookupExplain x (_:ls) =  lookupExplain (x-1) ls 


newtype Table k v = T [(k, v)]

table :: Table String Doc
table = 
    T [
        (
            "MissingMainClass",
            "Welcome to the Encore Compiler!" $$
            "Here you will meet many wonderful methods and functions and whatnot!"
        )
    ]

{-
error <- case lookupHash (head $ words $ show err) of
    Nothing -> return "Error: "
    Just num -> return $ printf "Error[E%04d]: " num
-}