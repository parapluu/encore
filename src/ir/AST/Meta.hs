module AST.Meta where

import Text.Parsec(SourcePos, sourceLine, sourceColumn)
import Data.Maybe

import Types

data CaptureStatus = Free
                   | Captured
                     deriving (Eq, Show)

data MetaInfo = Closure {metaId :: String}
              | Async {metaId :: String}
                deriving (Eq, Show)

data Meta a = Meta {sourcePos :: SourcePos,
                    metaType  :: Maybe Type,
                    sugared   :: Maybe a,
                    captureStatus :: Maybe CaptureStatus,
                    metaInfo  :: Maybe MetaInfo} deriving (Eq, Show)

meta :: SourcePos -> Meta a
meta pos = Meta {sourcePos = pos
                ,metaType = Nothing
                ,sugared = Nothing
                ,captureStatus = Nothing
                ,metaInfo = Nothing}

getPos :: Meta a -> SourcePos
getPos = sourcePos

getLine :: Meta a -> Int
getLine = sourceLine . sourcePos

getCol :: Meta a -> Int
getCol = sourceColumn . sourcePos

setType :: Type -> Meta a -> Meta a
setType newType m = m {metaType = Just newType}

getType :: Meta a -> Type
getType m = fromMaybe err (metaType m)
    where
      err = error "Meta.hs: No type given to node"

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

isFree :: Meta a -> Bool
isFree m = captureStatus m == Just Free

isCaptured :: Meta a -> Bool
isCaptured m = captureStatus m == Just Captured

makeFree :: Meta a -> Meta a
makeFree m = m{captureStatus = Just Free}

makeCaptured :: Meta a -> Meta a
makeCaptured m = m{captureStatus = Just Captured}