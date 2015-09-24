{-|
Module      : Neko.IO
Description : Neko-styled IO operations
Copyright   : (c) Petr Penzin, 2015
License     : BSD3
Maintainer  : penzin.dev@gmail.com
Stability   : experimental
Portability : cross-platform

A set of functions to parse Haskell types from and represent in Neko binary.

-}
module Neko.IO where

import Data.ByteString.Lazy as BS
import Data.Maybe
import Data.Bits
import Data.Word
import Data.Int

-- | Read an arbitrary-sized integer, Neko style.
--   Note: Do we need to hande endianness?
readVarInt :: Int64 -- ^ Size in bytes
           -> ByteString -- ^ bytestring to read from
           -> Maybe (Integer, ByteString) -- ^ Number and unconsumed bytestring on success
readVarInt size bs = if (BS.length chunk < size) then Nothing else Just (fuse 0 (BS.unpack chunk), rest)
    where chunk = BS.take size bs
          rest = BS.drop size bs
          fuse :: Integer -> [Word8] -> Integer
          fuse _ [] = 0
          fuse pos (w:ws) = ((fromIntegral w) `shiftL` (fromIntegral $ pos * 8)) + fuse (pos + 1) ws

-- | Read an arbitrary-sized unsigned integer, Neko style.
--   Note: Do we need to hande endianness?
readVarUInt :: Int64 -- ^ Size in bytes
            -> ByteString -- ^ bytestring to read from
            -> Maybe (Word, ByteString) -- ^ Number and unconsumed bytestring on success
readVarUInt size bs = if (isNothing signed) then Nothing else Just ((fromInteger num), rest)
    where signed = readVarInt size bs
          (num, rest) = fromJust signed

-- | Read a 32-bit integer, Neko style.
readInt32 :: ByteString -- ^ bytestring to read from
          -> Maybe (Int32, ByteString) -- ^ Number and unconsumed bytestring on success
readInt32 bs = if (isNothing r) then Nothing else Just (fromIntegral num, rest)
    where r = readVarInt 4 bs
          (num, rest) = fromJust r

-- | Read a 16-bit unsigned integer, Neko style.
readUInt16 :: ByteString -- ^ bytestring to read from
           -> Maybe (Word16, ByteString) -- ^ Number and unconsumed bytestring on success
readUInt16 bs = if (isNothing r) then Nothing else Just (fromIntegral num, rest)
    where r = readVarUInt 2 bs
          (num, rest) = fromJust r

