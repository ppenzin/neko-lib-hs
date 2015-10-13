{-|
Module      : Globals.Read
Description : Read Global entries
Copyright   : (c) Petr Penzin, 2015
License     : BSD2
Maintainer  : penzin.dev@gmail.com
Stability   : stable
Portability : cross-platform

Binary read test for Global entries in a module

-}
module Globals.Read where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Data.ByteString.Lazy as B

import Neko.Bytecode.Globals

globalsReadTests = testGroup "Globals READ tests"
  [ SC.testProperty "Read string constant" $
      (readGlobals 1 $ pack [0x03, 0x0d, 0x00, 0x48, 0x65, 0x6c, 0x6c, 0x6f,  0x20, 0x77, 0x6f, 0x72, 0x6c, 0x64, 0x21, 0x0a])
                                                                  == Just ([GlobalString "Hello world!\n"], B.empty)
  , SC.testProperty "Read global variable" $
      (readGlobals 1 $ pack [0x01, 0x48, 0x65, 0x00])
                                                                  == Just ([GlobalVar "He"], B.empty)
  ]

