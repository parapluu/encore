module AST.Meta where

import Text.Parsec(SourcePos, sourceLine, sourceColumn)
import Data.Maybe

import Types
import Identifiers

data CaptureStatus = Captured
                   | Free
                     deriving (Eq, Show)

data MetaInfo = Closure {metaId :: String}
              | Async {metaId :: String}
              | MetaArrow {metaArrow :: Type}
              | EnvChange {bindings :: [(Name, Type)]}
              | CondEnvChange {bindings :: [(Name, Type)]}
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

getMetaArrowType :: Meta a -> Type
getMetaArrowType = metaArrow . fromJust . metaInfo

setMetaArrowType :: Type -> Meta a -> Meta a
setMetaArrowType ty m
    | isArrowType ty = m{metaInfo = Just $ MetaArrow ty}
    | otherwise = error $ "Meta.hs: Tried to set arrow type to " ++
                          showWithKind ty

isFree :: Meta a -> Bool
isFree m = captureStatus m == Just Free

isCaptured :: Meta a -> Bool
isCaptured m = captureStatus m == Just Captured

makeFree :: Meta a -> Meta a
makeFree m = m{captureStatus = Just Free}

makeCaptured :: Meta a -> Meta a
makeCaptured m = m{captureStatus = Just Captured}

hasEnvChange :: Meta a -> Bool
hasEnvChange Meta{metaInfo = Just EnvChange{}} = True
hasEnvChange _ = False

hasCondEnvChange :: Meta a -> Bool
hasCondEnvChange Meta{metaInfo = Just CondEnvChange{}} = True
hasCondEnvChange _ = False

getEnvChange :: Meta a -> [(Name, Type)]
getEnvChange = bindings . fromJust . metaInfo

setEnvChange :: [(Name, Type)] -> Meta a -> Meta a
setEnvChange bindings m = m{metaInfo = Just $ EnvChange bindings}

setCondEnvChange :: [(Name, Type)] -> Meta a -> Meta a
setCondEnvChange bindings m = m{metaInfo = Just $ CondEnvChange bindings}