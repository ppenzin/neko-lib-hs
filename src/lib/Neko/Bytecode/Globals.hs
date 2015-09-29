{-|
Module      : Neko.Bytecode.Globals
Description : Global values for a Neko module
Copyright   : (c) Petr Penzin, 2015
License     : BSD2
Maintainer  : penzin.dev@gmail.com
Stability   : experimental
Portability : cross-platform

Emit and parse global values for a Neko module.

-}
module Neko.Bytecode.Globals where

import Data.ByteString.Lazy as BS
import Data.ByteString.Lazy.Char8 as BSChar
import Data.Binary.Get
import Data.Maybe
import Data.Word
import Data.Int

import Neko.IO

-- | Global value type
data Global =
        GlobalVar String -- ^ Global variable (name)
      | GlobalFunction (Int, Int) -- ^ Function
      | GlobalString String -- ^ String literal
      | GlobalFloat String -- ^ Floating point constant
      | GlobalDebug ([String], [(Int, Int)]) -- ^ Debug information
      | GlobalVersion Int -- ^ Version record
      deriving (Show, Eq)

-- | Read globals from a bytestring
readGlobals :: Int32 -- ^ Number of global values to read
            -> ByteString -- ^ Bytestring to read from
            -> Maybe ([Global], ByteString) -- ^ On success: list of global values read and unconsumed bytestring
readGlobals 0 bs = Just ([], bs)
readGlobals n bs = if (isNothing current) then Nothing else res
    where current = readGlobal bs
          (val, rest) = fromJust current
          next = readGlobals (n - 1) rest
          res = if (isNothing next) then Nothing else Just ((val:vals), theTail)
          (vals, theTail) = fromJust next

-- | Read a single global value, if succesfull return a single global value and remaining bytestring
readGlobal :: ByteString -- ^ Bytes to read from
           -> Maybe (Global, ByteString) -- ^ Read value and the rest of bytes (if successfull)
readGlobal bs = if (isPrefixOf (BS.singleton 1) bs) then (readGlobalVar rest) else
                if (isPrefixOf (BS.singleton 2) bs) then error "TODO readGlobal: implement GlobalFunction" else
                if (isPrefixOf (BS.singleton 3) bs) then (readGlobalString rest) else
                if (isPrefixOf (BS.singleton 4) bs) then error "TODO readGlobal: implement GlobalFloat" else
                if (isPrefixOf (BS.singleton 5) bs) then (readDebugInfo rest) else
                if (isPrefixOf (BS.singleton 6) bs) then error "TODO readGlobal: implement GlobalVersion" else
                Nothing
    where rest = BS.drop 1 bs

-- | Read global string literal form a bytestring
readGlobalString :: ByteString -- ^ Bytes to read from
                 -> Maybe (Global, ByteString) -- ^ Value read and remaining bytes (if succesfull)
readGlobalString bs = if (isNothing maybeLength) then Nothing else
                      if (BS.length subByteString < length) then Nothing else
                      Just (GlobalString (BSChar.unpack subByteString), ret)
    where maybeLength = readUInt16 bs
          (len, rest) = fromJust maybeLength
          length = (fromIntegral len)::Int64
          subByteString = BS.take length rest
          ret = BS.drop length rest

-- | Read debug information literal form a bytestring
readDebugInfo :: ByteString -- ^ Bytes to read from
                 -> Maybe (Global, ByteString) -- ^ Value read and remaining bytes (if succesfull)
readDebugInfo bs = error "TODO readGlobal: implement GlobalDebug"

-- | Read a global variable
readGlobalVar :: ByteString -- ^ Bytes to read from
                 -> Maybe (Global, ByteString) -- ^ Value read and remaining bytes (if succesfull)
readGlobalVar bs = Just (GlobalVar name, rest)
    where (name, rest) = readNullTerminatedString bs

-- | Get a list of globals from a bytestring
getGlobals :: Word32 -- ^ number of globals
           -> Get [Global] -- ^ decode a list
getGlobals 0 = return []
getGlobals n = getGlobal >>= \g -> getGlobals (n - 1) >>= \gs -> return (g:gs)

-- | Read a global from a bytestring
getGlobal :: Get Global
getGlobal = getWord8
        >>= \b -> if (b == 1) then getGlobalVar else
                  if (b == 2) then error "TODO getGlobal: implement GlobalFunction" else
                  if (b == 3) then getGlobalString else
                  if (b == 4) then error "TODO getGlobal: implement GlobalFloat" else
                  if (b == 5) then error "TODO getGlobal: implement GlobalDebug" else
                  if (b == 6) then error "TODO getGlobal: implement GlobalVersion" else
                  fail "getGlobal: urecognized global"

-- | Decode a global variable
getGlobalVar :: Get Global
getGlobalVar = getLazyByteStringNul >>= \b -> return ( GlobalVar $ BSChar.unpack b)

-- | Decode a global string
getGlobalString :: Get Global
getGlobalString = getWord16le
              >>= \length -> getLazyByteString (fromIntegral length)
              >>= \s -> return (GlobalString $ BSChar.unpack s)
