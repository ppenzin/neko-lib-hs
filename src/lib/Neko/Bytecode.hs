module Neko.Bytecode where

import Data.ByteString.Lazy as BS
import Data.ByteString.Lazy.Char8 as BSChar
import Data.Maybe
import Data.Either
import Data.Word
import Data.Int

import Neko.IO

data Instruction = 
        -- getters
        AccNull
      | AccTrue
      | AccFalse
      | AccThis
      | AccInt Int
      | AccStack Int
      | AccGlobal Int
      | AccEnv Int
      | AccField String
      | AccArray
      | AccIndex Int
      | AccBuiltin String
        -- setters
      | SetStack Int
      | SetGlobal Int
      | SetEnv Int
      | SetField String
      | SetArray
      | SetIndex Int
      | SetThis
        -- stack ops
      | Push
      | Pop Int
      | Call Int
      | ObjCall Int
      | Jump Int
      | JumpIf Int
      | JumpIfNot Int
      | Trap Int
      | EndTrap
      | Ret Int
      | MakeEnv Int
      | MakeArray Int
        -- value ops
      | Bool
      | IsNull
      | IsNotNull
      | Add
      | Sub
      | Mult
      | Div
      | Mod
      | Shl
      | Shr
      | UShr
      | Or
      | And
      | Xor
      | Eq
      | Neq
      | Gt
      | Gte
      | Lt
      | Lte
      | Not
        -- extra ops
      | TypeOf
      | Compare
      | Hash
      | New
      | JumpTable Int
      | Apply Int
      | AccStack0
      | AccStack1
      | AccIndex0
      | AccIndex1
      | PhysCompare
      | TailCall (Int, Int)
      | Loop
      deriving (Show, Eq)

data Global =
        GlobalVar String
      | GlobalFunction (Int, Int)
      | GlobalString String
      | GlobalFloat String
      | GlobalDebug ([String], [(Int, Int)])
      | GlobalVersion Int
      deriving (Show, Eq)

data Module = N {globals::[Global], code::[Instruction]} deriving (Show, Eq)

-- Return module or return an error
readModule :: ByteString -> Either String Module
readModule bs = if (isNothing afterMagic) then (Left "Failed to read magic value")
                else readModuleData $ fromJust afterMagic
    where afterMagic = stripMagic bs

-- Process module without magic value
readModuleData :: ByteString -> Either String Module
readModuleData bs = if (isNothing moduleFields) then (Left err) else Right N {globals=[], code=[]}
    where (rest, err, moduleFields) = readModuleFields bs

-- Read fields that determine code size, number of globals, etc
readModuleFields :: ByteString -> (ByteString, String, Maybe (Int32, Int32, Int32))
readModuleFields bs = if (isNothing resNumGlobals) then (bs, errNumGlobals, Nothing) else
                      if (isNothing resNumFields) then (bs, errNumFields, Nothing) else
                      if (isNothing resCodeSize) then (bs, errCodeSize, Nothing) else
                      if (isJust checkError) then (bs, fromJust checkError, Nothing) else
                      (bsAfter, "Success", Just (numGlobals, numFields, codeSize))
                    where
                      resNumGlobals = readInt32 bs
                      (numGlobals, afterNumGlobals) = fromJust resNumGlobals
                      resNumFields = readInt32 afterNumGlobals
                      (numFields, afterNumFields) = fromJust resNumFields
                      resCodeSize = readInt32 afterNumFields
                      (codeSize, bsAfter) = fromJust resCodeSize
                      errNumGlobals = "Failed to read number of globals"
                      errNumFields = "Failed to read number of fields"
                      errCodeSize = "Failed to read code size"
                      checkError = checkModuleFields numGlobals numFields codeSize

-- Check that module fields have valid values
checkModuleFields :: Int32 -> Int32 -> Int32 -> Maybe String
checkModuleFields globals fields code
     = if (globals < 0 || globals > 0xFFFF) then Just "Number of globals not between 0 and 0xFFFF" else
       if (fields < 0 || fields > 0xFFFF) then Just "Number of fields not between 0 and 0xFFFF" else
       if (code < 0 || code > 0xFFFFFF) then Just "Code size not between 0 and 0xFFFFFF" else
       Nothing

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

-- Check first four bytes for magic value. Return the rest of the string if OK, otherwise return nothing
stripMagic :: ByteString -> Maybe ByteString
stripMagic bs = if (isPrefixOf (BSChar.pack "NEKO") bs) then (Just $ BS.drop 4 bs) else Nothing

