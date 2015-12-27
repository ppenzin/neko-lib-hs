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
            else if (opnum == 6) then return (AccGlobal $ fromIntegral $ fromJust arg)
            else if (opnum == 9) then return (AccArray)
            else if (opnum == 11) then
                        if (member (fromJust arg) ids)
                        then return (AccBuiltin (fromJust $ H.lookup (fromJust arg) ids))
                        else fail ("Field not found for AccBuiltin (" ++ (showHex (fromJust arg) "") ++ ")")
            else if (opnum == 16) then return (SetArray)
            else if (opnum == 19) then return (Push)
            else if (opnum == 21) then return (Call $ fromIntegral $ fromJust arg)
            else fail "getInstruction: unrecognized opcode"

-- | Get integer opcode
opcode :: Instruction -- ^ Instruction to process
       -> (Word8, Maybe Word32) -- ^ Opcode and additional argument
opcode (AccNull)      = (0,  Nothing)
opcode (AccTrue)      = (1,  Nothing)
opcode (AccFalse)     = (2,  Nothing)
opcode (AccThis)      = (3,  Nothing)
opcode (AccGlobal n)  = (6,  Just $ fromIntegral n)
opcode (AccArray)     = (9,  Nothing)
opcode (AccBuiltin s) = (11, Just $ hash s)
opcode (SetArray)     = (16, Nothing)
opcode (Push)         = (19, Nothing)
opcode (Call n)       = (21, Just $ fromIntegral n)
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
