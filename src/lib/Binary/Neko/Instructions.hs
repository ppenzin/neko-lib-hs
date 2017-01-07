{-|
Module      : Binary.Neko.Instructions
Description : Emit and parse Binary.Neko instructions
Copyright   : (c) Petr Penzin, 2015
License     : BSD2
Maintainer  : penzin.dev@gmail.com
Stability   : experimental
Portability : cross-platform

Types and primitives to deal with Binary.Neko instructions

-}
module Binary.Neko.Instructions where

import Data.Int
import Data.Bits
import Data.Word
import Data.Maybe
import Data.Either
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy as BS
import Numeric (showHex)

import Binary.Neko.Hashtbl as H

-- | Various NekoVM instructions
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

-- | Read instructions
-- Consume bytestring, produce instructions and status message
readInstructions :: Word32 -- ^ code size
                 -> Hashtbl -- ^ context (names of fields)
                 -> ByteString -- ^ bytes to read from
                 -> (ByteString, String, Maybe [Instruction]) -- ^ unconsumed input, status message and list of instructions
readInstructions n ids bs = if (isRight res) then (rest, "Success", Just (is)) else (rest', err, Nothing)
    where res = runGetOrFail (getInstructions n ids) bs
          Right (rest, _, is) = res
          Left (rest', _, err) = res

-- | Read a single bytecode instruction
readInstruction :: Hashtbl -- ^ Names of fieds for the module
                -> ByteString -- ^ Input
                -> (Maybe Instruction, ByteString) -- ^ Result or nothing, unconsumed input
readInstruction ids bs = if (isRight res) then (Just (i), rest) else (Nothing, rest')
    where res = runGetOrFail (getInstruction ids) bs
          Right (rest, _, i) = res
          Left (rest', _, _) = res

-- | Grab instructions from a bytestring
-- Decode bytestring, consuming one byte per instruction with no paramenters
-- and two for instructions with parameters
getInstructions :: Word32 -- ^ code size - number of instructions+arguments left to parse
                -> Hashtbl -- ^ Builtins hashtable to provide context
                -> Get [Instruction] -- ^ decoder
getInstructions 0 _ = return []
getInstructions n ids = if (n < 0) then fail "Stepped over code size" else getInstruction ids
                    >>= \i -> getInstructions (n - (if (hasParam i) then 2 else 1)) ids
                    >>= \is -> return (i:is)

-- | Grab a single instruction from a bytestring
-- Some instruction acces filds by using hashes of the names, therefore
-- require a hash table with field names.
getInstruction :: Hashtbl -- ^ Builtins hashtable for getting names
               -> Get Instruction -- ^ Instruction parser
getInstruction ids
             = getWord8
           >>= \b -> return (b .&. 3)
           >>= \code ->  if (code == 0) then getOp (b `shiftR` 2) Nothing ids
                    else if (code == 1) then getOp (b `shiftR` 3) (Just $ fromIntegral $ b `shiftR` 2 .&. 1) ids
                    else if (code == 2) then
                             getWord8 >>= \w -> if (b == 2) then (getOp (fromIntegral w) Nothing ids)
                                                else (getOp (b `shiftR` 2) (Just (fromIntegral w)) ids)
                    else if (code == 3) then
                             getWord32le >>= \i -> getOp (b `shiftR` 2) (Just i) ids
                    else fail "getInstruction: unrecognized opcode group"

-- | Second level of instruction read logic
getOp :: Word8 -- ^ Operation number
      -> Maybe Word32 -- ^ Additional argument
      -> Hashtbl -- ^ Some instructions require access to builtins hashtable
      -> Get Instruction -- ^ Instruction parser
getOp opnum arg ids
               = if (opnum == 0) then return (AccNull)
            else if (opnum == 1) then return (AccTrue)
            else if (opnum == 2) then return (AccFalse)
            else if (opnum == 3) then return (AccThis)
            else if (opnum == 4) then return (AccInt $ fromIntegral $ fromJust arg)
            else if (opnum == 5) then return (AccStack $ (fromIntegral $ fromJust arg) + 2)
            else if (opnum == 6) then return (AccGlobal $ fromIntegral $ fromJust arg)
            else if (opnum == 9) then return (AccArray)
            else if (opnum == 11) then
                        if (member (fromJust arg) ids)
                        then return (AccBuiltin (fromJust $ H.lookup (fromJust arg) ids))
                        else fail ("Field not found for AccBuiltin (" ++ (showHex (fromJust arg) "") ++ ")")
            else if (opnum == 16) then return (SetArray)
            else if (opnum == 19) then return (Push)
            else if (opnum == 21) then return (Call $ fromIntegral $ fromJust arg)
            else if (opnum == 27) then return (EndTrap)
            else if (opnum == 31) then return (Bool)
            else if (opnum == 32) then return (IsNull)
            else if (opnum == 33) then return (IsNotNull)
            else if (opnum == 34) then return (Add)
            else if (opnum == 35) then return (Sub)
            else if (opnum == 36) then return (Mult)
            else if (opnum == 37) then return (Div)
            else if (opnum == 38) then return (Mod)
            else if (opnum == 39) then return (Shl)
            else if (opnum == 40) then return (Shr)
            else if (opnum == 41) then return (UShr)
            else if (opnum == 42) then return (Or)
            else if (opnum == 43) then return (And)
            else if (opnum == 44) then return (Xor)
            else if (opnum == 45) then return (Eq)
            else if (opnum == 46) then return (Neq)
            else if (opnum == 47) then return (Gt)
            else if (opnum == 48) then return (Gte)
            else if (opnum == 49) then return (Lt)
            else if (opnum == 50) then return (Lte)
            else if (opnum == 51) then return (Not)
            else if (opnum == 52) then return (TypeOf)
            else if (opnum == 53) then return (Compare)
            else if (opnum == 54) then return (Hash)
            else if (opnum == 55) then return (New)
            else if (opnum == 58) then return (AccStack0)
            else if (opnum == 59) then return (AccStack1)
            else if (opnum == 60) then return (AccIndex0)
            else if (opnum == 61) then return (AccIndex1)
            else if (opnum == 62) then return (PhysCompare)
            else fail "getInstruction: unrecognized opcode"

-- | Get integer opcode
opcode :: Instruction -- ^ Instruction to process
       -> (Word8, Maybe Word32) -- ^ Opcode and additional argument
opcode (AccNull)      = (0,  Nothing)
opcode (AccTrue)      = (1,  Nothing)
opcode (AccFalse)     = (2,  Nothing)
opcode (AccThis)      = (3,  Nothing)
opcode (AccInt n)     = (4,  Just $ fromIntegral n)
opcode (AccStack n)   = (5,  Just $ (fromIntegral n) - 2) -- TODO Do we need to check for n>=2 ?
opcode (AccGlobal n)  = (6,  Just $ fromIntegral n)
opcode (AccArray)     = (9,  Nothing)
opcode (AccBuiltin s) = (11, Just $ hash s)
opcode (SetArray)     = (16, Nothing)
opcode (Push)         = (19, Nothing)
opcode (Call n)       = (21, Just $ fromIntegral n)
opcode (EndTrap)      = (27, Nothing)
opcode (Bool)         = (31, Nothing)
opcode (IsNull)       = (32, Nothing)
opcode (IsNotNull)    = (33, Nothing)
opcode (Add)          = (34, Nothing)
opcode (Sub)          = (35, Nothing)
opcode (Mult)         = (36, Nothing)
opcode (Div)          = (37, Nothing)
opcode (Mod)          = (38, Nothing)
opcode (Shl)          = (39, Nothing)
opcode (Shr)          = (40, Nothing)
opcode (UShr)         = (41, Nothing)
opcode (Or)           = (42, Nothing)
opcode (And)          = (43, Nothing)
opcode (Xor)          = (44, Nothing)
opcode (Eq)           = (45, Nothing)
opcode (Neq)          = (46, Nothing)
opcode (Gt)           = (47, Nothing)
opcode (Gte)          = (48, Nothing)
opcode (Lt)           = (49, Nothing)
opcode (Lte)          = (50, Nothing)
opcode (Not)          = (51, Nothing)
opcode (TypeOf)       = (52, Nothing)
opcode (Compare)      = (53, Nothing)
opcode (Hash)         = (54, Nothing)
opcode (New)          = (55, Nothing)
opcode (AccStack0)    = (58, Nothing)
opcode (AccStack1)    = (59, Nothing)
opcode (AccIndex0)    = (60, Nothing)
opcode (AccIndex1)    = (61, Nothing)
opcode (PhysCompare)  = (62, Nothing)
opcode x              = error ("TODO implement " ++ (show x))

-- | Write instruction out using Put monad
putInstruction :: Instruction -> Put
putInstruction i
    = let (op, arg) = opcode i
          n = fromJust arg
      in if (isNothing arg) then (putWord8 $ op `shiftL` 2)
    else if (op < 32 && (n == 0 || n == 1)) then (putWord8 $ (op `shiftL` 3) .|. ((fromIntegral n) `shiftL` 2) .|. 1)
    else if (n >= 0 && n <= 0xFF) then (putWord8 ((op `shiftL` 2) .|. 2) >> putWord8 (fromIntegral n))
    else (putWord8 ((op `shiftL` 2) .|. 3) >> putWord32le n)

-- | Write a few instructions out using Put monad
putInstructions :: [Instruction] -> Put
putInstructions [] = return ()
putInstructions (i:is) = putInstruction i >> putInstructions is

-- | Determine whether instruction has a parameter
hasParam :: Instruction -> Bool
hasParam (AccInt _)     = True
hasParam (AccStack _)   = True
hasParam (AccGlobal _)  = True
hasParam (AccEnv _)     = True
hasParam (AccField _)   = True
hasParam (AccIndex _)   = True
hasParam (AccBuiltin _) = True
hasParam (SetStack _)   = True
hasParam (SetGlobal _)  = True
hasParam (SetEnv _)     = True
hasParam (SetField _)   = True
hasParam (SetIndex _)   = True
hasParam (Pop _)        = True
hasParam (Call _)       = True
hasParam (ObjCall _)    = True
hasParam (Jump _)       = True
hasParam (JumpIf _)     = True
hasParam (JumpIfNot _)  = True
hasParam (Trap _)       = True
hasParam (Ret _)        = True
hasParam (MakeEnv _)    = True
hasParam (MakeArray _)  = True
hasParam (JumpTable _)  = True
hasParam (Apply _)      = True
hasParam (TailCall _)   = True
hasParam _              = False
