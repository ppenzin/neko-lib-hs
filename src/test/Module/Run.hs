{-|
Module      : Module.Run
Description : Tests that execute Neko bytecode
Copyright   : (c) Petr Penzin, 2015
License     : BSD2
Module.Runtainer  : penzin.dev@gmail.com
Stability   : stable
Portability : cross-platform

Execute Neko bytecode to check compatibility

-}
module Module.Run where 

import Test.Tasty
import Test.Tasty.HUnit as HU


-- | Bytecode execution tests
runBytecodeTests :: FilePath -- ^ Path to neko executable 
                 -> TestTree -- ^ Resulting test tree
runBytecodeTests nekoExe = testCase "TODO implement bytecode tests, neko is " $ assertFailure nekoExe
