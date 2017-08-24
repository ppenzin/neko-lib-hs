{-|
Module      : Instructions.Read
Description : Test reading instructions
Copyright   : (c) Petr Penzin, 2015
License     : BSD2
Maintainer  : penzin.dev@gmail.com
Stability   : stable
Portability : cross-platform

Binary read tests for instructions

-}
module Instructions.Read where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Data.ByteString.Lazy as B
import Data.Binary.Get

import Binary.Neko.Instructions
import Binary.Neko.Hashtbl as H

-- | Test reading a single instruction (no string literals) from a byte string
readInstr i bs = readInstrWithStrings i H.empty bs

-- | Test reading a single instruction with string literal hashes
readInstrWithStrings i hashes bs = SC.testProperty (show i) $ (readInstruction hashes bs) == ((Just i), B.empty)

-- | Entry point for instruction read tests
instrReadTests = testGroup "Instructions READ tests"
  [ readInstr AccNull $ pack [0x00]
  , readInstr AccTrue $ pack [0x04]
  , readInstr AccFalse $ pack [0x08]
  , readInstr AccThis $ pack [0x0C]
  , readInstr (AccInt 7) $ pack [0x12, 0x07]
  , readInstr (AccInt 0x100) $ pack [0x13, 0x00, 0x01, 0x00, 0x00]
  , readInstr (AccStack 3) $ pack [0x2d]
  , readInstr (AccGlobal 0) $ pack [0x31]
  , readInstr (AccEnv 1) $ pack [0x3d]
  , readInstrWithStrings (AccField "print") (H.fromStringList ["print"]) $ pack [0x23, 0x2d, 0x58, 0x8b, 0xc8]
  , SC.testProperty "AccField -- wrong hash" $ (runGetOrFail (getInstruction (H.fromStringList ["print"])) $ pack [0x23, 0x2d, 0x58, 0x8b, 0xFF]) == (Left (B.empty ,5,"Field not found for AccField (ff8b582d)"))
  , SC.testProperty "AccField -- missing field" $ (runGetOrFail (getInstruction H.empty) $ pack [0x23, 0x2d, 0x58, 0x8b, 0xc8]) == (Left (B.empty ,5,"Field not found for AccField (c88b582d)"))
  , readInstr AccArray $ pack [0x24]
  , readInstr (AccIndex 3) $ pack [0x55]
  , readInstr SetArray $ pack [0x40]
  , readInstr Push $ pack [0x4c]
  , readInstrWithStrings (AccBuiltin "print") (H.fromStringList ["print"]) $ pack [0x2f, 0x2d, 0x58, 0x8b, 0xc8]
  , SC.testProperty "AccBuiltin -- wrong hash" $ (runGetOrFail (getInstruction (H.fromStringList ["print"])) $ pack [0x2f, 0x2d, 0x58, 0x8b, 0xFF]) == (Left (B.empty ,5,"Field not found for AccBuiltin (ff8b582d)"))
  , SC.testProperty "AccBuiltin -- missing field" $ (runGetOrFail (getInstruction H.empty) $ pack [0x2f, 0x2d, 0x58, 0x8b, 0xc8]) == (Left (B.empty ,5,"Field not found for AccBuiltin (c88b582d)"))
  , readInstr (SetStack 3) $ pack [0x32, 0x03]
  , readInstr (SetGlobal 0) $ pack [0x69]
  , readInstr (SetEnv 1) $ pack [0x75]
  , readInstrWithStrings (SetField "print") (H.fromStringList ["print"]) $ pack [0x3f, 0x2d, 0x58, 0x8b, 0xc8]
  , SC.testProperty "SetField -- wrong hash" $ (runGetOrFail (getInstruction (H.fromStringList ["print"])) $ pack [0x3f, 0x2d, 0x58, 0x8b, 0xFF]) == (Left (B.empty ,5,"Field not found for SetField (ff8b582d)"))
  , SC.testProperty "SetField -- missing field" $ (runGetOrFail (getInstruction H.empty) $ pack [0x3f, 0x2d, 0x58, 0x8b, 0xc8]) == (Left (B.empty ,5,"Field not found for SetField (c88b582d)"))
  , readInstr (SetIndex 3) $ pack [0x46, 0x03]
  , readInstr (SetThis) $ pack [0x48]
  , readInstr (Pop 1) $ pack [0xa5]
  , readInstr (Call 1) $ pack [0xad]
  , readInstr (ObjCall 2) $ pack [0x5a, 0x02]
  , readInstr (Jump 10) $ pack [0x5e, 0x0a]
  , readInstr (JumpIf 9) $ pack [0x62, 0x09]
  , readInstr (JumpIfNot 8) $ pack [0x66, 0x08]
  , readInstr (Trap 7) $ pack [0x6a, 0x07]
  , readInstr EndTrap $ pack [0x6c]
  , readInstr (Ret 6) $ pack [0x72, 0x06]
  , readInstr (MakeEnv 4) $ pack [0x76, 0x04]
  , readInstr (MakeArray 24) $ pack [0x7a, 0x18]
  , readInstr Bool $ pack [0x7c]
  , readInstr IsNull $ pack [0x80]
  , readInstr IsNotNull $ pack [0x84]
  , readInstr Add $ pack [0x88]
  , readInstr Sub $ pack [0x8c]
  , readInstr Mult $ pack [0x90]
  , readInstr Div $ pack [0x94]
  , readInstr Mod $ pack [0x98]
  , readInstr Shl $ pack [0x9c]
  , readInstr Shr $ pack [0xa0]
  , readInstr UShr $ pack [0xa4]
  , readInstr Or $ pack [0xa8]
  , readInstr And $ pack [0xac]
  , readInstr Xor $ pack [0xb0]
  , readInstr Eq $ pack [0xb4]
  , readInstr Neq $ pack [0xb8]
  , readInstr Gt $ pack [0xbc]
  , readInstr Gte $ pack [0xc0]
  , readInstr Lt $ pack [0xc4]
  , readInstr Lte $ pack [0xc8]
  , readInstr Not $ pack [0xcc]
  , readInstr TypeOf $ pack [0xd0]
  , readInstr Compare $ pack [0xd4]
  , readInstr Hash $ pack [0xd8]
  , readInstr New $ pack [0xdc]
  , readInstr (JumpTable 8) $ pack [0xe2, 0x8]
  , readInstr (Apply 5) $ pack [0xe6, 0x5]
  , readInstr AccStack0 $ pack [0xe8]
  , readInstr AccStack1 $ pack [0xec]
  , readInstr AccIndex0 $ pack [0xf0]
  , readInstr AccIndex1 $ pack [0xf4]
  , readInstr PhysCompare $ pack [0xf8]
  , readInstr (TailCall (5,3)) $ pack [0xfe, 0x1d]
  , readInstr Loop $ pack [0x02, 0x40]
  ]

