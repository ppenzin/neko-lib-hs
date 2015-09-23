module Neko.Bytecode.Globals where

import Data.ByteString.Lazy as BS
import Data.ByteString.Lazy.Char8 as BSChar
import Data.Maybe
import Data.Int

import Neko.IO

data Global =
        GlobalVar String
      | GlobalFunction (Int, Int)
      | GlobalString String
      | GlobalFloat String
      | GlobalDebug ([String], [(Int, Int)])
      | GlobalVersion Int
      deriving (Show, Eq)

-- Read globals
readGlobals :: Integer -> ByteString -> Maybe ([Global], ByteString)
readGlobals 0 bs = Just ([], bs)
readGlobals n bs = if (isNothing current) then Nothing else res
    where current = readGlobal bs
          (val, rest) = fromJust current
          next = readGlobals (n - 1) rest
          res = if (isNothing next) then Nothing else Just ((val:vals), theTail)
          (vals, theTail) = fromJust next

-- Read fields

-- Read instructions

-- Read a single global value, if succesfull return a single global value and remaining bytestring
readGlobal :: ByteString -> Maybe (Global, ByteString)
readGlobal bs = if (isPrefixOf (BS.singleton 1) bs) then Just (GlobalVar "TODO", empty) else -- TODO
                if (isPrefixOf (BS.singleton 2) bs) then Just (GlobalFunction (0, 0), empty) else -- TODO
                if (isPrefixOf (BS.singleton 3) bs) then (readGlobalString rest) else
                if (isPrefixOf (BS.singleton 4) bs) then Just (GlobalFloat "TODO", empty) else -- TODO
                if (isPrefixOf (BS.singleton 5) bs) then Just (GlobalDebug (["TODO"], []), empty) else -- TODO
                if (isPrefixOf (BS.singleton 6) bs) then Just (GlobalVersion 0, empty) else -- TODO
                Nothing
    where rest = BS.drop 1 bs

-- Read global string
readGlobalString :: ByteString -> Maybe (Global, ByteString)
readGlobalString bs = if (isNothing maybeLength) then Nothing else
                      if (BS.length subByteString < length) then Nothing else
                      Just (GlobalString (BSChar.unpack subByteString), ret)
    where maybeLength = readUInt16 bs
          (len, rest) = fromJust maybeLength
          length = (fromIntegral len)::Int64
          subByteString = BS.take length rest
          ret = BS.drop length rest

