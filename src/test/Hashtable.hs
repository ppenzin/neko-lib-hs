{-|
Module      : Hashtable
Description : Hashtable tests
Copyright   : (c) Petr Penzin, 2015
License     : BSD2
Maintainer  : penzin.dev@gmail.com
Stability   : stable
Portability : cross-platform

Tests for the Hashtbl data type

-}
module Hashtable where

import Test.Tasty
import Test.Tasty.SmallCheck as SC

import Neko.Hashtbl as H

-- Happy path test :(
hashTests = testGroup "Hashtbl tests"
  [ SC.testProperty "Simple hash (of string 'print')" $ H.hash "print" == 0xC88B582D
  ]

