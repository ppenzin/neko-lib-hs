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
import Data.Maybe
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
readInstructions :: Int32 -- ^ code size
                 -> ByteString -- ^ bytes to read from
                 -> (ByteString, String, Maybe [Instruction]) -- ^ unconsumed input, status message and list of instructions
readInstructions 0 bs = (bs, "Success", Just [])
readInstructions n bs = if (isNothing current) then (bs, "Failed to read instruction", Nothing)  else
                        if (isNothing resRest) then (bs, err, Nothing)  else (rest, "Success", Just (i:is))
    where (current, remByteStr) = readInstruction bs
          (rest, err, resRest) = readInstructions (n - 1) remByteStr
          i = fromJust current
          is = fromJust resRest
        

-- | Read a single bytecode instruction
readInstruction :: ByteString -- ^ Input
                -> (Maybe Instruction, ByteString) -- ^ Result or nothing, unconsumed input
readInstruction bs = (instr, rest)
    where firstByte = BS.head bs
          firstTail = BS.tail bs
          code = firstByte .&. 3
          opNum = if (code == 0) then (firstByte `shiftR` 2) else
                  if (code == 1) then (firstByte `shiftR` 3) else
                  if (code == 2) then if (firstByte == 2) then (BS.head firstTail) else (firstByte `shiftR` 2) else
                  if (code == 3) then (firstByte `shiftR` 2) else error "Unrecognized operation"
          opcodeTail = if (firstByte == 2) then BS.tail firstTail else firstTail
          instr = if (opNum == 6) then Just (AccGlobal $ fromIntegral ((firstByte `shiftR` 2) .&. 1)) else
                  if (opNum == 19) then Just (Push) else
                  --if (opNum == 11) then Just (AccBuiltin ) else
                  --if (opNum == 21) then Just (Call ) else
                  Nothing
          rest = if (isNothing instr) then bs else opcodeTail
