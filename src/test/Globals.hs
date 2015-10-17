{-|
Module      : Globals
Description : Misc Global tests
Copyright   : (c) Petr Penzin, 2015
License     : BSD2
Maintainer  : penzin.dev@gmail.com
Stability   : stable
Portability : cross-platform

Globals tests that don't involve processing binary or IO, generally checks that
can be performed without diving on binary level

-}
module Globals where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Data.ByteString.Lazy as B
import Data.Binary.Get
import Data.Binary.Put

import Binary.Neko.Globals

binaryCheckGlobals = testGroup "produce/consume global"
  [ checkGlobal $ GlobalVar "foo"
  , checkGlobal $ GlobalString "XYZ"
  ]

-- | Assemble and dissassemble a global
checkGlobal global
    = SC.testProperty ("Check binary read/write for " ++ (show global)) $ (runGet getGlobal $ runPut $ putGlobal global) == global

