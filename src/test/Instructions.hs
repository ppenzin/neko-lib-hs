{-|
Module      : Instructions
Description : Test instructions
Copyright   : (c) Petr Penzin, 2015
License     : BSD2
Maintainer  : penzin.dev@gmail.com
Stability   : stable
Portability : cross-platform

Generic tests for instructions

-}
module Instructions where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Data.ByteString.Lazy as B
import Data.Binary.Get
import Data.Binary.Put

import Binary.Neko.Instructions
import Binary.Neko.Hashtbl as H

binaryCheckInstructions = testGroup "produce/consume instruction"
  [ checkInstruction H.empty $ AccNull
  , checkInstruction H.empty $ AccTrue
  , checkInstruction H.empty $ AccFalse
  , checkInstruction H.empty $ AccThis
  , checkInstruction H.empty $ AccArray
  , checkInstruction H.empty $ AccGlobal 1
  , checkInstruction (H.fromStringList ["someBuiltin"]) $ AccBuiltin "someBuiltin"
  , checkInstruction H.empty $ SetArray
  , checkInstruction H.empty Push
  , checkInstruction H.empty $ Call 0
  ]

-- | Assemble and dissassemble an instruction
checkInstruction ids instruction
    = SC.testProperty ("Check binary read/write for " ++ (show instruction)) $ (runGet (getInstruction ids) $ runPut $ putInstruction instruction) == instruction

