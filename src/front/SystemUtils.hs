{-# LANGUAGE CPP, ScopedTypeVariables #-}
--
-- Copyright (C) 2004 Don Stewart - http://www.cse.unsw.edu.au/~dons
--
-- This library is free software; you can redistribute it and/or
-- modify it under the terms of the GNU Lesser General Public
-- License as published by the Free Software Foundation; either
-- version 2.1 of the License, or (at your option) any later version.
--
-- This library is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public
-- License along with this library; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
-- USA
--

-- This module has was created from System.Plugins.Utils, but has
-- had certain functions (with dependencies) removed.

module SystemUtils (
    Arg,

    hWrite,

    dropSuffix,
    mkModid,
    changeFileExt,
    joinFileExt,
    splitFileExt,

    isSublistOf,                -- :: Eq a => [a] -> [a] -> Bool

    dirname,
    basename,

    (</>), (<.>), (<+>), (<>),

    newer,

    encode,
    decode,
    EncodedString,

    panic

  ) where

import Control.Exception               (IOException, catch)
import Data.Char
import Data.List
import Prelude hiding                  (catch)

import System.IO
import System.Environment           ( getEnv )
import System.Directory             ( doesFileExist, getModificationTime, removeFile )

-- ---------------------------------------------------------------------
-- some misc types we use

type Arg = String

-- ---------------------------------------------------------------------
-- | useful
--
panic s = ioError ( userError s )

-- ---------------------------------------------------------------------
-- | writeFile for Handles
--
hWrite :: Handle -> String -> IO ()
hWrite hdl src = hPutStr hdl src >> hClose hdl >> return ()


-- ---------------------------------------------------------------------
-- some filename manipulation stuff

--
-- | </>, <.> : join two path components
--
infixr 6 </>
infixr 6 <.>

(</>), (<.>), (<+>), (<>) :: FilePath -> FilePath -> FilePath
[] </> b = b
a  </> b = a ++ "/" ++ b

[] <.> b = b
a  <.> b = a ++ "." ++ b

[] <+> b = b
a  <+> b = a ++ " " ++ b

[] <> b = b
a  <> b = a ++ b

--
-- | dirname : return the directory portion of a file path
-- if null, return "."
--
dirname :: FilePath -> FilePath
dirname p  =
    let x = findIndices (== '\\') p
        y = findIndices (== '/') p
    in
    if not $ null x
      then if not $ null y
          then if (maximum x) > (maximum y) then dirname' '\\' p else dirname' '/' p
          else dirname' '\\' p
      else dirname' '/' p
    where
        dirname' chara pa =
            case reverse $ dropWhile (/= chara) $ reverse pa of
                [] -> "."
                pa' -> pa'

--
-- | basename : return the filename portion of a path
--
basename :: FilePath -> FilePath
basename p =
    let x = findIndices (== '\\') p
        y = findIndices (== '/') p
    in
    if not $ null x
      then if not $ null y
          then if (maximum x) > (maximum y) then basename' '\\' p else basename' '/' p
          else basename' '\\' p
      else basename' '/' p
    where
        basename' chara pa = reverse $ takeWhile (/= chara) $ reverse pa

--
-- drop suffix
--
dropSuffix :: FilePath -> FilePath
dropSuffix f = reverse . tail . dropWhile (/= '.') $ reverse f

--
-- | work out the mod name from a filepath
mkModid :: String -> String
mkModid = (takeWhile (/= '.')) . reverse . (takeWhile (\x -> ('/'/= x) && ('\\' /= x))) . reverse


-----------------------------------------------------------
-- Code from Cabal ----------------------------------------

-- | Changes the extension of a file path.
changeFileExt :: FilePath           -- ^ The path information to modify.
              -> String             -- ^ The new extension (without a leading period).
                                    -- Specify an empty string to remove an existing
                                    -- extension from path.
              -> FilePath           -- ^ A string containing the modified path information.
changeFileExt fpath ext = joinFileExt name ext
  where
    (name,_) = splitFileExt fpath

-- | The 'joinFileExt' function is the opposite of 'splitFileExt'.
-- It joins a file name and an extension to form a complete file path.
--
-- The general rule is:
--
-- > filename `joinFileExt` ext == path
-- >   where
-- >     (filename,ext) = splitFileExt path
joinFileExt :: String -> String -> FilePath
joinFileExt fpath ""  = fpath
joinFileExt fpath ext = fpath ++ '.':ext

-- | Split the path into file name and extension. If the file doesn\'t have extension,
-- the function will return empty string. The extension doesn\'t include a leading period.
--
-- Examples:
--
-- > splitFileExt "foo.ext" == ("foo", "ext")
-- > splitFileExt "foo"     == ("foo", "")
-- > splitFileExt "."       == (".",   "")
-- > splitFileExt ".."      == ("..",  "")
-- > splitFileExt "foo.bar."== ("foo.bar.", "")
splitFileExt :: FilePath -> (String, String)
splitFileExt p =
  case break (== '.') fname of
        (suf@(_:_),_:pre) -> (reverse (pre++fpath), reverse suf)
        _                 -> (p, [])
  where
    (fname,fpath) = break isPathSeparator (reverse p)

-- | Checks whether the character is a valid path separator for the host
-- platform. The valid character is a 'pathSeparator' but since the Windows
-- operating system also accepts a slash (\"\/\") since DOS 2, the function
-- checks for it on this platform, too.
isPathSeparator :: Char -> Bool
isPathSeparator ch =
#if defined(CYGWIN) || defined(__MINGW32__)
  ch == '/' || ch == '\\'
#else
  ch == '/'
#endif

-- Code from Cabal end ------------------------------------
-----------------------------------------------------------


------------------------------------------------------------------------

--
-- | is file1 newer than file2?
--
-- needs some fixing to work with 6.0.x series. (is this true?)
--
-- fileExist still seems to throw exceptions on some platforms: ia64 in
-- particular.
--
-- invarient : we already assume the first file, 'a', exists
--
newer :: FilePath -> FilePath -> IO Bool
newer a b = do
    aT      <- getModificationTime a
    bExists <- doesFileExist b
    if not bExists
        then return True                -- needs compiling
        else do bT <- getModificationTime b
                return ( aT > bT )    -- maybe need recompiling

------------------------------------------------------------------------
--
-- | return the Z-Encoding of the string.
--
-- Stolen from GHC. Use -package ghc as soon as possible
--
type EncodedString = String

encode :: String -> EncodedString
encode []     = []
encode (c:cs) = encodeCh c ++ encode cs

unencodedChar :: Char -> Bool   -- True for chars that don't need encoding
unencodedChar 'Z' = False
unencodedChar 'z' = False
unencodedChar c   =  c >= 'a' && c <= 'z'
                  || c >= 'A' && c <= 'Z'
                  || c >= '0' && c <= '9'

--
-- Decode is used for user printing.
--
decode :: EncodedString -> String
decode [] = []
decode ('Z' : d : rest) | isDigit d = decodeTuple   d rest
                        | otherwise = decodeUpper   d : decode rest
decode ('z' : d : rest) | isDigit d = decodeNumEsc d rest
                        | otherwise = decodeLower   d : decode rest
decode (c  : rest) = c : decode rest

decodeUpper, decodeLower :: Char -> Char

decodeUpper 'L' = '('
decodeUpper 'R' = ')'
decodeUpper 'M' = '['
decodeUpper 'N' = ']'
decodeUpper 'C' = ':'
decodeUpper 'Z' = 'Z'
decodeUpper ch  = error $ "decodeUpper can't handle this char `"++[ch]++"'"

decodeLower 'z' = 'z'
decodeLower 'a' = '&'
decodeLower 'b' = '|'
decodeLower 'c' = '^'
decodeLower 'd' = '$'
decodeLower 'e' = '='
decodeLower 'g' = '>'
decodeLower 'h' = '#'
decodeLower 'i' = '.'
decodeLower 'l' = '<'
decodeLower 'm' = '-'
decodeLower 'n' = '!'
decodeLower 'p' = '+'
decodeLower 'q' = '\''
decodeLower 'r' = '\\'
decodeLower 's' = '/'
decodeLower 't' = '*'
decodeLower 'u' = '_'
decodeLower 'v' = '%'
decodeLower ch  = error $ "decodeLower can't handle this char `"++[ch]++"'"

-- Characters not having a specific code are coded as z224U
decodeNumEsc :: Char -> [Char] -> String
decodeNumEsc d cs
  = go (digitToInt d) cs
  where
    go n (c : rest) | isDigit c = go (10*n + digitToInt c) rest
    go n ('U' : rest)           = chr n : decode rest
    go _ other = error $
        "decodeNumEsc can't handle this: \""++other++"\""


encodeCh :: Char -> EncodedString
encodeCh c | unencodedChar c = [c]     -- Common case first

-- Constructors
encodeCh '('  = "ZL"   -- Needed for things like (,), and (->)
encodeCh ')'  = "ZR"   -- For symmetry with (
encodeCh '['  = "ZM"
encodeCh ']'  = "ZN"
encodeCh ':'  = "ZC"
encodeCh 'Z'  = "ZZ"

-- Variables
encodeCh 'z'  = "zz"
encodeCh '&'  = "za"
encodeCh '|'  = "zb"
encodeCh '^'  = "zc"
encodeCh '$'  = "zd"
encodeCh '='  = "ze"
encodeCh '>'  = "zg"
encodeCh '#'  = "zh"
encodeCh '.'  = "zi"
encodeCh '<'  = "zl"
encodeCh '-'  = "zm"
encodeCh '!'  = "zn"
encodeCh '+'  = "zp"
encodeCh '\'' = "zq"
encodeCh '\\' = "zr"
encodeCh '/'  = "zs"
encodeCh '*'  = "zt"
encodeCh '_'  = "zu"
encodeCh '%'  = "zv"
encodeCh c    = 'z' : shows (ord c) "U"

decodeTuple :: Char -> EncodedString -> String
decodeTuple d cs
  = go (digitToInt d) cs
  where
    go n (c : rest) | isDigit c = go (10*n + digitToInt c) rest
    go 0 ['T']          = "()"
    go n ['T']          = '(' : replicate (n-1) ',' ++ ")"
    go 1 ['H']          = "(# #)"
    go n ['H']          = '(' : '#' : replicate (n-1) ',' ++ "#)"
    go _ other = error $ "decodeTuple \'"++other++"'"

-- ---------------------------------------------------------------------

--
-- 'isSublistOf' takes two arguments and returns 'True' iff the first
-- list is a sublist of the second list. This means that the first list
-- is wholly contained within the second list. Both lists must be
-- finite.

isSublistOf :: Eq a => [a] -> [a] -> Bool
isSublistOf [] _ = True
isSublistOf _ [] = False
isSublistOf x y@(_:ys)
    | isPrefixOf x y = True
    | otherwise      = isSublistOf x ys
