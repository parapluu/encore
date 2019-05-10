module AST.Meta where

import Text.Megaparsec(unPos, SourcePos(..))
import Data.Maybe
import Text.Printf

import Types

data CaptureStatus = Captured
                   | Free
                     deriving (Eq, Show)

data MetaInfo = Closure {metaId :: String}
              | Async {metaId :: String}
              | MetaArrow {metaArrow :: Type}
                deriving (Eq, Show)

data Position = SingletonPos {startPos :: SourcePos}
              | RangePos {startPos :: SourcePos,
                          endPos :: SourcePos}
                deriving (Eq)

instance Show Position where
  show (SingletonPos s) = showSourcePos s 
  show (RangePos s e) = 
    sourceName s ++ " " ++
    show (unPos $ sourceLine s, unPos $ sourceColumn s) ++ " to " ++
    show (unPos $ sourceLine e, unPos $ sourceColumn e)
    --(showSourcePos s) ++ " to " ++ (showSourcePos e)

newPos :: SourcePos -> Position
newPos = SingletonPos

data Meta a = Meta {position  :: Position,
                    metaType  :: Maybe Type,
                    sugared   :: Maybe a,
                    captureStatus :: Maybe CaptureStatus,
                    isPattern :: Bool,
                    statement :: Bool,
                    metaInfo  :: Maybe MetaInfo} deriving (Eq, Show)

meta :: Position -> Meta a
meta position =
    Meta {position
         ,metaType = Nothing
         ,sugared = Nothing
         ,statement = False
         ,captureStatus = Nothing
         ,isPattern = False
         ,metaInfo = Nothing}

setEndPos :: SourcePos -> Meta a -> Meta a
setEndPos endPos m@Meta{position} =
  m{position = RangePos{startPos = startPos position, endPos}}

showSourcePos :: SourcePos -> String
showSourcePos pos =
  let line = unPos (sourceLine pos)
      col = unPos (sourceColumn pos)
      file = sourceName pos
  in printf "%s (line %d, column %d)" (show file) line col

showPos :: Meta a -> String
showPos = showSourcePos . startPos . position

getPos :: Meta a -> Position
getPos = position

setType :: Type -> Meta a -> Meta a
setType newType m = m {metaType = Just newType}

getType :: Meta a -> Type
getType m = fromMaybe err (metaType m)
    where
      err = error $ "Meta.hs: No type given to node at " ++ showPos m

setSugared :: a -> Meta a -> Meta a
setSugared s m = m {sugared = Just s}

getSugared :: Meta a -> Maybe a
getSugared = sugared

metaClosure :: String -> Meta a -> Meta a
metaClosure id m = m {metaInfo = Just $ Closure id}

metaTask :: String -> Meta a -> Meta a
metaTask id m = m {metaInfo = Just $ Async id}

getMetaId :: Meta a -> String
getMetaId = metaId . fromJust . metaInfo

getMetaArrowType :: Meta a -> Type
getMetaArrowType = metaArrow . fromJust . metaInfo

setMetaArrowType :: Type -> Meta a -> Meta a
setMetaArrowType ty m
    | isArrowType ty = m{metaInfo = Just $ MetaArrow ty}
    | otherwise = error $ "Meta.hs: Tried to set arrow type to " ++
                          showWithKind ty

isStat :: Meta a -> Bool
isStat Meta{statement} = statement

makeStat :: Meta a -> Meta a
makeStat m@Meta{statement} = m{statement=True}

isFree :: Meta a -> Bool
isFree m = captureStatus m == Just Free

isCaptured :: Meta a -> Bool
isCaptured m = captureStatus m == Just Captured

makeFree :: Meta a -> Meta a
makeFree m = m{captureStatus = Just Free}

makeCaptured :: Meta a -> Meta a
makeCaptured m = m{captureStatus = Just Captured}

makePattern :: Meta a -> Meta a
makePattern m = m{isPattern = True}
