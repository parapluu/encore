module AST.Meta where

import Text.Parsec(SourcePos, sourceLine, sourceColumn)

import Identifiers

data Meta = Meta {sourcePos :: SourcePos, ty :: Type} deriving (Eq, Show)

meta :: SourcePos -> Meta
meta pos = Meta {sourcePos = pos, ty = emptyType}

getLine :: Meta -> Int
getLine = sourceLine . sourcePos

getCol :: Meta -> Int
getCol = sourceColumn . sourcePos

setType :: Type -> Meta -> Meta
setType newType m = m {ty = newType}

getType :: Meta -> Type
getType = ty