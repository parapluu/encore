module AST.Meta where

import Text.Parsec(SourcePos, sourceLine, sourceColumn)
import Data.Maybe

import Identifiers
import Types

data MetaInfo = Unspecified
              | Closure {metaId :: String}
                deriving (Eq, Show)

data Meta a = Meta {sourcePos :: SourcePos, 
                    metaType  :: Type, 
                    sugared   :: Maybe a,
                    metaInfo  :: MetaInfo} deriving (Eq, Show)

meta :: SourcePos -> Meta a
meta pos = Meta {sourcePos = pos, metaType = emptyType, sugared = Nothing, metaInfo = Unspecified}

getPos :: Meta a -> SourcePos
getPos = sourcePos

getLine :: Meta a -> Int
getLine = sourceLine . sourcePos

getCol :: Meta a -> Int
getCol = sourceColumn . sourcePos

setType :: Type -> Meta a -> Meta a
setType newType m = m {metaType = newType}

getType :: Meta a -> Type
getType = metaType

setSugared :: a -> Meta a -> Meta a
setSugared s m = m {sugared = Just s}

getSugared :: Meta a -> Maybe a
getSugared = sugared

metaClosure :: String -> Meta a -> Meta a
metaClosure id m = m {metaInfo = Closure id}

getMetaId :: Meta a -> String
getMetaId = metaId . metaInfo