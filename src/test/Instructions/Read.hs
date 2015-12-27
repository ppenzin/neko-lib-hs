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
  , readInstr AccArray $ pack [0x24]
  , readInstr (AccGlobal 0) $ pack [0x31]
  , readInstr SetArray $ pack [0x40]
  , readInstr Push $ pack [0x4c]
  , readInstrWithStrings (AccBuiltin "print") (H.fromStringList ["print"]) $ pack [0x2f, 0x2d, 0x58, 0x8b, 0xc8]
  , SC.testProperty "AccBuiltin -- wrong hash" $ (runGetOrFail (getInstruction (H.fromStringList ["print"])) $ pack [0x2f, 0x2d, 0x58, 0x8b, 0xFF]) == (Left (B.empty ,5,"Field not found for AccBuiltin (ff8b582d)"))
  , SC.testProperty "AccBuiltin -- missing field" $ (runGetOrFail (getInstruction H.empty) $ pack [0x2f, 0x2d, 0x58, 0x8b, 0xc8]) == (Left (B.empty ,5,"Field not found for AccBuiltin (c88b582d)"))
  , readInstr (Call 1) $ pack [0xad]
  ]

