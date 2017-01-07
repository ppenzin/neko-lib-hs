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
  , checkInstruction H.empty $ AccInt 7
  , checkInstruction H.empty $ AccInt 1000
  , checkInstruction H.empty $ AccStack 3
  , checkInstruction H.empty $ AccGlobal 1
  , checkInstruction H.empty $ AccEnv 4
  , checkInstruction (H.fromStringList ["someField"]) $ AccField "someField"
  , checkInstruction H.empty $ AccArray
  , checkInstruction H.empty $ AccIndex 5
  , checkInstruction (H.fromStringList ["someBuiltin"]) $ AccBuiltin "someBuiltin"
  , checkInstruction H.empty $ SetStack 3
  , checkInstruction H.empty $ SetGlobal 1
  , checkInstruction H.empty $ SetEnv 4
  , checkInstruction (H.fromStringList ["foobar"]) $ SetField "foobar"
  , checkInstruction H.empty $ SetArray
  , checkInstruction H.empty $ SetIndex 7
  , checkInstruction H.empty $ SetThis
  , checkInstruction H.empty $ Push
  , checkInstruction H.empty $ Pop 10
  , checkInstruction H.empty $ Call 0
  , checkInstruction H.empty $ EndTrap
  , checkInstruction H.empty $ Bool
  , checkInstruction H.empty $ IsNull
  , checkInstruction H.empty $ IsNotNull
  , checkInstruction H.empty $ Add
  , checkInstruction H.empty $ Sub
  , checkInstruction H.empty $ Mult
  , checkInstruction H.empty $ Div
  , checkInstruction H.empty $ Mod
  , checkInstruction H.empty $ Shl
  , checkInstruction H.empty $ Shr
  , checkInstruction H.empty $ UShr
  , checkInstruction H.empty $ Or
  , checkInstruction H.empty $ And
  , checkInstruction H.empty $ Xor
  , checkInstruction H.empty $ Eq
  , checkInstruction H.empty $ Neq
  , checkInstruction H.empty $ Gt
  , checkInstruction H.empty $ Gte
  , checkInstruction H.empty $ Lt
  , checkInstruction H.empty $ Lte
  , checkInstruction H.empty $ Not
  , checkInstruction H.empty $ TypeOf
  , checkInstruction H.empty $ Compare
  , checkInstruction H.empty $ Hash
  , checkInstruction H.empty $ New
  , checkInstruction H.empty $ AccStack0
  , checkInstruction H.empty $ AccStack1
  , checkInstruction H.empty $ AccIndex0
  , checkInstruction H.empty $ AccIndex1
  , checkInstruction H.empty $ PhysCompare
  ]

-- | Assemble and dissassemble an instruction
checkInstruction ids instruction
    = SC.testProperty ("Check binary read/write for " ++ (show instruction)) $ (runGet (getInstruction ids) $ runPut $ putInstruction instruction) == instruction

