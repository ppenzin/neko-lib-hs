{-|
Module      : Neko.Bytecode
Description : Emit and parse Neko bytecode
Copyright   : (c) Petr Penzin, 2015
License     : BSD2
Maintainer  : penzin.dev@gmail.com
Stability   : experimental
Portability : cross-platform

Primitives to emit and parse Neko bytecode, including instruction definitions.

-}
module Neko.Bytecode where

import Data.ByteString.Lazy as BS
import Data.ByteString.Lazy.Char8 as BSChar
import Data.Binary.Get
import Data.Maybe
import Data.Either
import Data.Word
import Data.Int

import Neko.IO
import Neko.Bytecode.Globals
import Neko.Bytecode.Instructions

-- | A Neko module. Consists of global entities and a list of instructions
data Module = N {globals::[Global], fields::[String], code::[Instruction]} deriving (Show, Eq)

-- | Parse module from ByteString.
--   Return module or return an error string
readModule :: ByteString -> Either String Module
readModule bs = if (isRight res) then
                    if (BS.null rest) then (Right m) else Left "Trailing bytes"
                else Left err
    where res = runGetOrFail getModule bs
          Right (rest, _, m) = res
          Left (_, _, err) = res

-- | Internal type for module header (counts of entities in the module)
data ModuleHeader = ModuleHeader {
    numGlobals :: Word32, -- ^ Number of globals
    numFields  :: Word32, -- ^ Number of fields
    codeSize   :: Word32  -- ^ Code size (number of instructions
}

-- | Pick module header fields from a bytestring. Requires bytestring to start with the first field.
getModuleHeader :: Get ModuleHeader
getModuleHeader = ModuleHeader <$> getWord32le <*> getWord32le <*> getWord32le

-- | Get a full module from a bytestring
getModule :: Get Module
getModule = getMagicCheck
        >>= \good -> if (not good) then fail "Invalid magic value" else getModuleHeader
        >>= \h -> getModuleContents (numGlobals h) (numFields h) (codeSize h)

-- | Parse insides of a module from a bytestring.
--   Bytesting is expected to start with the first section of the module.
getModuleContents :: Word32 -- ^ number of globals
                  -> Word32 -- ^ number of fields
                  -> Word32 -- ^ code size
                  -> Get Module -- ^ decode module
getModuleContents globals fields code
     = if (globals > 0xFFFF) then fail "Number of globals not between 0 and 0xFFFF" else
       if (fields > 0xFFFF) then fail "Number of fields not between 0 and 0xFFFF" else
       if (code > 0xFFFFFF) then fail "Code size not between 0 and 0xFFFFFF" else
       N <$> (getGlobals globals) <*> (getFields fields) <*> (getInstructions code)
     
-- | A check for next four bytes matching neko magic value
getMagicCheck :: Get Bool
getMagicCheck = getLazyByteString 4 >>= \b -> return (b == BSChar.pack "NEKO")

-- | Grab a global field from a bytestring
getField :: Get String
getField = getLazyByteStringNul >>= \b -> return (BSChar.unpack b)

-- | Grab a list of fields (of known length) from a bytestring
getFields :: Word32 -> Get [String]
getFields 0 = return []
getFields n = getField >>= \s -> getFields (n - 1) >>= \ss -> return (s:ss)
