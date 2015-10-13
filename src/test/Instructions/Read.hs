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

import Neko.Bytecode.Instructions
import Neko.Hashtbl as H

instrReadTests = testGroup "Instructions READ tests"
  [ SC.testProperty "AccGlobal 0" $ (readInstruction H.empty $ pack [0x31]) == ((Just (AccGlobal 0)), B.empty)
  , SC.testProperty "Push" $ (readInstruction H.empty $ pack [0x4c]) == (Just (Push), B.empty)
  , SC.testProperty "AccBuiltin \"print\"" $ (readInstruction (H.fromStringList ["print"]) $ pack [0x2f, 0x2d, 0x58, 0x8b, 0xc8]) == (Just (AccBuiltin "print"), B.empty)
  , SC.testProperty "AccBuiltin -- wrong hash" $ (runGetOrFail (getInstruction (H.fromStringList ["print"])) $ pack [0x2f, 0x2d, 0x58, 0x8b, 0xFF]) == (Left (B.empty ,5,"Field not found for AccBuiltin (ff8b582d)"))
  , SC.testProperty "AccBuiltin -- missing field" $ (runGetOrFail (getInstruction H.empty) $ pack [0x2f, 0x2d, 0x58, 0x8b, 0xc8]) == (Left (B.empty ,5,"Field not found for AccBuiltin (c88b582d)"))
  , SC.testProperty "Call 1" $ (readInstruction H.empty $ pack [0xad]) == (Just (Call 1), B.empty)
  ]

