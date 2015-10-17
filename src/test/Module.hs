{-|
Module      : Module
Description : Module tests
Copyright   : (c) Petr Penzin, 2015
License     : BSD2
Maintainer  : penzin.dev@gmail.com
Stability   : stable
Portability : cross-platform

Generic (representation-independent) module tests

-}
module Module where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Data.ByteString.Lazy as B
import Data.Binary.Get
import Data.Binary.Put

import Binary.Neko.Module
import Binary.Neko.Instructions
import Binary.Neko.Globals
import Binary.Neko.Hashtbl as H

binaryCheckModules = testGroup "produce/consume module"
  [ checkModule "Hello world" $ N {globals=[GlobalString "Hello world!\n"], fields=H.fromStringList["print"], code=[AccGlobal 0, Push, AccBuiltin "print", Call 1]}
  ]

-- | Assemble and disassemble entire module
checkModule desc mod
    = SC.testProperty desc $ (runGet getModule $ runPut $ putModule mod) == mod
