module AST.Meta where

import Text.Parsec(SourcePos, sourceLine, sourceColumn)

import Identifiers
import Types

data Meta = Meta {sourcePos :: SourcePos, metaType :: Type, metaId :: String} deriving (Eq, Show)

meta :: SourcePos -> Meta
meta pos = Meta {sourcePos = pos, metaType = emptyType, metaId = ""}

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

setMetaId :: String -> Meta -> Meta
setMetaId id m = m {metaId = id}

getMetaId :: Meta -> String
getMetaId = metaId