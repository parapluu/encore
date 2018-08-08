module LSP.Data.Position (
  Position,
  Range,
  inRange,
  fromSourcePos,
  fromSourcePosRange,
  rangeCompliment,
  widestRange
) where

-- ###################################################################### --
-- Section: Imports
-- ###################################################################### --

-- Library
import Text.Megaparsec(SourcePos,unPos,sourceLine,sourceColumn)
import Data.List as List

-- ###################################################################### --
-- Section: Data
-- ###################################################################### --

{-  -}
type Position = (Int, Int)

{-  -}
type Range = (Position, Position)

-- ###################################################################### --
-- Section: Functions
-- ###################################################################### --

{-  -}
inRange :: Position -> Range -> Bool
inRange pos (start, end)
  | fst pos > fst start && fst pos < fst end = True
  | fst pos == fst start && fst pos < fst end && snd pos >= snd start = True
  | fst pos > fst start && fst pos == fst end && snd pos < snd end = True
  | fst pos == fst start && fst pos == fst end && snd pos >= snd start && snd pos < snd end = True
  | True = False -- sorry, had to do it (otherwise == True)

{-  -}
fromSourcePos :: SourcePos -> Position
fromSourcePos pos = (fromIntegral (unPos (sourceLine pos)), fromIntegral (unPos (sourceColumn pos)))

{-  -}
fromSourcePosRange :: SourcePos -> SourcePos -> Range
fromSourcePosRange from to = (fromSourcePos from, fromSourcePos to)

posLess :: Position -> Position -> Bool
posLess a b
  | fst a < fst b = True
  | fst a == fst b && snd a < snd b = True
  | otherwise = False

posGreater :: Position -> Position -> Bool
posGreater a b
  | fst a > fst b = True
  | fst a == fst b && snd a > snd b = True
  | otherwise = False

rangeCompliment :: Range -> Range -> Range
rangeCompliment a b
  | posLess (fst a) (fst b) = (fst a, fst b)
  | posGreater (snd a) (snd b) = (snd b, snd a)
  | otherwise = a

minPosition :: [Position] -> Position
minPosition [] = (0, 0)
minPosition (x:[]) = x
minPosition (x:y:xs)
    | posLess x y = minPosition (x:xs)
    | otherwise = minPosition (y:xs)

maxPosition :: [Position] -> Position
maxPosition [] = (0, 0)
maxPosition (x:[]) = x
maxPosition (x:y:xs)
    | posGreater x y = maxPosition (x:xs)
    | otherwise = maxPosition (y:xs)

widestRange :: Range -> [Range] -> Range
widestRange firstRange ranges = do
  let wholeRange = (firstRange:ranges)
  (minPosition (fmap fst wholeRange), maxPosition (fmap snd wholeRange))
