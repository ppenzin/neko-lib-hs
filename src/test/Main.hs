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

-- Option to get Neko executable
newtype NekoExe = NekoExe (Maybe FilePath)
  deriving (Eq, Ord, Typeable)

instance IsOption NekoExe where
  defaultValue = NekoExe Nothing
  parseValue = parseNekoExe
  optionName = return "neko"
  optionHelp = return "Run neko bytecode, value -- path to Neko executable, empty sets it to `neko'"

parseNekoExe :: String -> Maybe NekoExe
parseNekoExe s = if (null s) then return (NekoExe (Just "neko")) else return (NekoExe (Just s))

main = defaultMainWithIngredients ings $
  askOption $ \(NekoExe nekoExe) ->
  testGroup "Tests" $
  defaultTests ++
  if (isNothing nekoExe)
    then []
    else [ runBytecodeTests $ fromJust nekoExe ]
  where
    ings =
      includingOptions [Option (Proxy :: Proxy NekoExe)] :
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

