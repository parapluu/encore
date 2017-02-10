module AST.Meta where

import Text.Megaparsec(unPos, SourcePos(..))
import Data.Maybe
import Text.Printf

import Types

data MetaInfo = Unspecified
              | Closure {metaId :: String}
              | Async {metaId :: String}
                deriving (Eq, Show)

data Meta a = Meta {sourcePos   :: SourcePos,
                    metaType    :: Maybe Type,
                    sugared     :: Maybe a,
                    metaInfo    :: MetaInfo} deriving (Eq, Show)

meta :: SourcePos -> Meta a
meta pos = Meta {sourcePos = pos
                ,metaType = Nothing
                ,sugared = Nothing
                ,metaInfo = Unspecified}

showSourcePos pos =
  let line = unPos (sourceLine pos)
      col = unPos (sourceColumn pos)
      file = sourceName pos
  in printf "%s (line %d, column %d)" (show file) line col

showPos :: Meta a -> String
showPos = showSourcePos . sourcePos

getPos :: Meta a -> SourcePos
getPos = sourcePos

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
metaClosure id m = m {metaInfo = Closure id}

metaTask :: String -> Meta a -> Meta a
metaTask id m = m {metaInfo = Async id}

getMetaId :: Meta a -> String
getMetaId = metaId . metaInfo
