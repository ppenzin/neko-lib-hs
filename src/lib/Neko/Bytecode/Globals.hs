{-|
Module      : Neko.Bytecode.Globals
Description : Global values for a Neko module
Copyright   : (c) Petr Penzin, 2015
License     : BSD3
Maintainer  : penzin.dev@gmail.com
Stability   : experimental
Portability : cross-platform

Emit and parse global values for a Neko module.

-}
module Neko.Bytecode.Globals where

import Data.ByteString.Lazy as BS
import Data.ByteString.Lazy.Char8 as BSChar
import Data.Maybe
import Data.Int

import Neko.IO

-- | Global value type
data Global =
        GlobalVar String -- ^ Variable
      | GlobalFunction (Int, Int) -- ^ Function
      | GlobalString String -- ^ String literal
      | GlobalFloat String -- ^ Floating point constant
      | GlobalDebug ([String], [(Int, Int)]) -- ^ Debug information
      | GlobalVersion Int -- ^ Version record
      deriving (Show, Eq)

-- | Read globals from a bytestring
readGlobals :: Integer -- ^ Number of global values to read
            -> ByteString -- ^ Bytestring to read from
            -> Maybe ([Global], ByteString) -- ^ On success: list of global values read and unconsumed bytestring
readGlobals 0 bs = Just ([], bs)
readGlobals n bs = if (isNothing current) then Nothing else res
    where current = readGlobal bs
          (val, rest) = fromJust current
          next = readGlobals (n - 1) rest
          res = if (isNothing next) then Nothing else Just ((val:vals), theTail)
          (vals, theTail) = fromJust next

-- Read fields

-- Read instructions

-- | Read a single global value, if succesfull return a single global value and remaining bytestring
readGlobal :: ByteString -- ^ Bytes to read from
           -> Maybe (Global, ByteString) -- ^ Read value and the rest of bytes (if successfull)
readGlobal bs = if (isPrefixOf (BS.singleton 1) bs) then error "TODO readGlobal: implement GlobalVar" else
                if (isPrefixOf (BS.singleton 2) bs) then error "TODO readGlobal: implement GlobalFunction" else
                if (isPrefixOf (BS.singleton 3) bs) then (readGlobalString rest) else
                if (isPrefixOf (BS.singleton 4) bs) then error "TODO readGlobal: implement GlobalFloat" else
                if (isPrefixOf (BS.singleton 5) bs) then error "TODO readGlobal: implement GlobalDebug" else
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

