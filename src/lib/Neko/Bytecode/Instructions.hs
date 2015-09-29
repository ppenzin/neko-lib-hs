{-|
Module      : Neko.Bytecode.Instructions
Description : Emit and parse Neko instructions
Copyright   : (c) Petr Penzin, 2015
License     : BSD2
Maintainer  : penzin.dev@gmail.com
Stability   : experimental
Portability : cross-platform

Types and primitives to deal with Neko instructions

-}
module Neko.Bytecode.Instructions where

import Data.Int
import Data.Bits
import Data.Word
import Data.Maybe
import Data.Either
import Data.Binary.Get
import Data.ByteString.Lazy as BS

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
                 -> ByteString -- ^ bytes to read from
                 -> (ByteString, String, Maybe [Instruction]) -- ^ unconsumed input, status message and list of instructions
readInstructions n bs = if (isRight res) then (rest, "Success", Just (is)) else (rest', err, Nothing)
    where res = runGetOrFail (getInstructions n) bs
          Right (rest, _, is) = res
          Left (rest', _, err) = res

-- | Read a single bytecode instruction
readInstruction :: ByteString -- ^ Input
                -> (Maybe Instruction, ByteString) -- ^ Result or nothing, unconsumed input
readInstruction bs = if (isRight res) then (Just (i), rest) else (Nothing, rest')
    where res = runGetOrFail getInstruction bs
          Right (rest, _, i) = res
          Left (rest', _, _) = res

-- | Grab instructions from a bytestring
getInstructions :: Word32 -- ^ number of instruction
                -> Get [Instruction] -- ^ decoder
getInstructions 0 = return []
getInstructions n = getInstruction
                >>= \i -> getInstructions (n - 1) 
                >>= \is -> return (i:is)

-- | Grab a single instruction from a bytestring
getInstruction :: Get Instruction
getInstruction = getWord8
             >>= \b -> return (b .&. 3)
             >>= \code ->      if (code == 0) then getOp (b `shiftR` 2) Nothing
                          else if (code == 1) then getOp (b `shiftR` 3) (Just $ fromIntegral $ b `shiftR` 2 .&. 1)
                          else if (code == 2) then
                                   getWord8 >>= \w -> if (b == 2) then (getOp (fromIntegral w) Nothing)
                                                      else (getOp (b `shiftR` 2) (Just (fromIntegral w)))
                          else if (code == 3) then
                                   getWord32le >>= \i -> getOp (b `shiftR` 2) (Just i)
                          else fail "getInstruction: unrecognized opcode group"

-- | Second level of instruction read logic
getOp :: Word8 -- ^ Operation number
      -> Maybe Word32 -- ^ Additional argument
      -> Get Instruction -- ^ Instruction parser
getOp opnum arg = if (opnum == 6) then return (AccGlobal $ fromIntegral $ fromJust arg) else
                  if (opnum == 19) then return (Push) else
                  fail "getInstruction: unrecognized opcode"
