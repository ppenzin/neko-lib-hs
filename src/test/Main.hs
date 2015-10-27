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
{-# LANGUAGE DeriveDataTypeable #-}
module Main where 

import Test.Tasty
import Test.Tasty.Options
import Test.Tasty.SmallCheck as SC
import Data.Typeable (Typeable)
import Data.Tagged
import Data.Proxy
import Data.Maybe
import Options.Applicative

-- Import test modules
import Module
import Module.Read
import Module.Run
import Globals
import Globals.Read
import Instructions
import Instructions.Read
import Hashtable

-- Option to enable Neko execution
newtype Neko = Neko Bool
  deriving (Eq, Ord, Typeable)

instance IsOption Neko where
  defaultValue = Neko False
  parseValue = fmap Neko . safeRead
  optionName = return "neko"
  optionHelp = return "Enable Binary.Neko runtime tests"
  optionCLParser = flagCLParser (Just 'n')(Neko True)

-- Option to get Neko executable
newtype NekoExe = NekoExe FilePath
  deriving (Eq, Ord, Typeable)

instance IsOption NekoExe where
  defaultValue = NekoExe ""
  parseValue = parseNekoExe
  optionName = return "neko-exe"
  optionHelp = return "Custom path to Binary.Neko executable, implies --neko"

parseNekoExe :: String -> Maybe NekoExe
parseNekoExe s = return (NekoExe s)

main = defaultMainWithIngredients ings $
  askOption $ \(NekoExe nekoExe) ->
  askOption $ \(Neko runNeko) ->
  testGroup "Tests" $
  defaultTests ++
  if ((not runNeko) && nekoExe == "")
    then []
    else [ runBytecodeTests $ if (null nekoExe) then "neko" else nekoExe]
  where
    ings =
      includingOptions [Option (Proxy :: Proxy NekoExe), Option (Proxy :: Proxy Neko)] :
      defaultIngredients

defaultTests :: [TestTree]
defaultTests = [readTests, hashTests, binaryCheckTests]

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

