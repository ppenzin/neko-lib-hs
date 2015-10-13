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

import Data.ByteString.Lazy as BS
import Data.Binary.Put
import System.Directory
import System.Process
import System.Random
import System.IO
import System.Exit

import Neko.Bytecode
import Neko.Hashtbl
import Neko.Bytecode.Globals
import Neko.Bytecode.Instructions


-- | Bytecode execution tests
runBytecodeTests :: FilePath -- ^ Path to neko executable 
                 -> TestTree -- ^ Resulting test tree
runBytecodeTests nekoExe = testGroup "Neko execution tests" 
 [ testCase "Hello world" $ 
     verifyHappy nekoExe (N {globals=[GlobalString "Hello world!\n"], fields=fromStringList["print"], code=[AccGlobal 0, Push, AccBuiltin "print", Call 1]}) "Hello world!\n"
 ]

-- | Wrapper for Neko interpreting a module
interp :: FilePath -- ^ Path to neko executable
       -> Module -- ^ Module to interpret 
       -> String -- ^ Standard input
       -> IO (ExitCode, String, String) -- ^ Exit code, stdout, stderr
interp exe mod input = randomName
                   >>= \(fileName, modName) -> openFile fileName WriteMode
                   >>= \handle -> hPut handle (runPut $ putModule mod)
                   >>  hClose handle
                   >>  readProcessWithExitCode exe [modName] input
                   >>= \(a,b,c) -> removeFile fileName
                   >>  return (a,b,c)

-- | Random file name
randomName :: IO (String, String) -- ^ Return file name and a module name
randomName = (randomIO :: IO Int) >>= \n -> return ("module" ++ show n ++ ".n", "module" ++ show n)

-- | Verify that test runs succesfully and prints the expected string to stdout (nothing on stderr)
verifyHappy :: FilePath -- ^ Path to Neko executable
            -> Module -- ^ Module to interpret
            -> String -- ^ Expected output
            -> Assertion -- ^ HUnit assertion
verifyHappy exe m expect = predicable @? "verifyHappy: test failure"
  where predicable = (interp exe m "") >>= \(exit, out, err) -> return (exit == ExitSuccess && out == expect && Prelude.null err)
