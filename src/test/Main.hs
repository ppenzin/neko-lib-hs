{-|
Module      : Main
Description : Test main
Copyright   : (c) Petr Penzin, 2015
License     : BSD2
Maintainer  : penzin.dev@gmail.com
Stability   : stable
Portability : cross-platform

Entry point for the test suite

-}
module Main where 

import Test.Tasty
import Test.Tasty.SmallCheck as SC

-- Import test modules
import Module
import Module.Read
import Globals
import Globals.Read
import Instructions
import Instructions.Read
import Hashtable

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [readTests, hashTests, binaryCheckTests]

readTests = testGroup "Binary read tests" 
  [ dasmTests
  , globalsReadTests
  , instrReadTests
  ]

binaryCheckTests = testGroup "Binary produce/consume tests"
  [ binaryCheckInstructions
  , binaryCheckGlobals
  , binaryCheckModules
  ]

