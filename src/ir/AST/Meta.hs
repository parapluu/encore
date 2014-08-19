module AST.Meta where

import Text.Parsec(SourcePos, sourceLine, sourceColumn)

import Identifiers
import Types

data MetaInfo = Unspecified
              | Closure {metaId :: String}
                deriving (Eq, Show)

data Meta = Meta {sourcePos :: SourcePos, metaType :: Type, metaInfo :: MetaInfo} deriving (Eq, Show)

meta :: SourcePos -> Meta
meta pos = Meta {sourcePos = pos, metaType = emptyType, metaInfo = Unspecified}

getPos :: Meta -> SourcePos
getPos = sourcePos

getLine :: Meta -> Int
getLine = sourceLine . sourcePos

getCol :: Meta -> Int
getCol = sourceColumn . sourcePos

setType :: Type -> Meta -> Meta
setType newType m = m {metaType = newType}

getType :: Meta -> Type
getType = metaType

metaClosure :: String -> Meta -> Meta
metaClosure id m = m {metaInfo = Closure id}

getMetaId :: Meta -> String
getMetaId = metaId . metaInfo