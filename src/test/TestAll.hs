import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Data.ByteString.Lazy

import Neko.Bytecode
import Neko.Bytecode.Globals
import Neko.Bytecode.Instructions

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [dasmTests, globalsReadTests, instrReadTests]

dasmTests = testGroup "Disassemble tests"
  [ SC.testProperty "Disassemble hello world" $
      (readModule hello) == Right (N {globals=[GlobalString "Hello world!\n", GlobalVar "var"], fields=["print"], code=[AccGlobal 0, Push, AccBuiltin "print", Call 1]})
  , SC.testProperty "Disassemble empty bytestring" $
      (readModule $ pack []) == Left "not enough bytes"
  , SC.testProperty "Invalid magic value" $
      (readModule $ pack [0x4f, 0x4b, 0x45, 0x4e, 0x02, 0x00, 0x00, 0x00]) == Left "Invalid magic value"
  , SC.testProperty "Too short to get globals" $
      (readModule $ pack [0x4e, 0x45, 0x4b, 0x4f, 0x02, 0x00])
                                                                  == Left "not enough bytes"
  , SC.testProperty "Too short to get fields" $
      (readModule $ pack [0x4e, 0x45, 0x4b, 0x4f, 0x02, 0x00, 0x00, 0x00,  0x01, 0x00])
                                                                  == Left "not enough bytes"
  , SC.testProperty "Too short to get code size" $
      (readModule $ pack [0x4e, 0x45, 0x4b, 0x4f, 0x02, 0x00, 0x00, 0x00,  0x01, 0x00, 0x00, 0x00, 0x07, 0x00])
                                                                  == Left "not enough bytes"
  , SC.testProperty "Invalid number of globals" $
      (readModule $ pack [0x4e, 0x45, 0x4b, 0x4f, 0xFF, 0xFF, 0xFF, 0xFF,  0x01, 0x00, 0x00, 0x00, 0x07, 0x00, 0x00, 0x00])
                                                                  == Left "Number of globals not between 0 and 0xFFFF"
  , SC.testProperty "Invalid number of fields" $
      (readModule $ pack [0x4e, 0x45, 0x4b, 0x4f, 0x02, 0x00, 0x00, 0x00,  0xFF, 0xFF, 0xFF, 0xFF, 0x07, 0x00, 0x00, 0x00])
                                                                  == Left "Number of fields not between 0 and 0xFFFF"
  , SC.testProperty "Invalid code size" $
      (readModule $ pack [0x4e, 0x45, 0x4b, 0x4f, 0x02, 0x00, 0x00, 0x00,  0x01, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0xFF])
                                                                  == Left "Code size not between 0 and 0xFFFFFF"
  ]

globalsReadTests = testGroup "Globals READ tests"
  [ SC.testProperty "Read string constant" $
      (readGlobals 1 $ pack [0x03, 0x0d, 0x00, 0x48, 0x65, 0x6c, 0x6c, 0x6f,  0x20, 0x77, 0x6f, 0x72, 0x6c, 0x64, 0x21, 0x0a])
                                                                  == Just ([GlobalString "Hello world!\n"], empty)
  , SC.testProperty "Read global variable" $
      (readGlobals 1 $ pack [0x01, 0x48, 0x65, 0x00])
                                                                  == Just ([GlobalVar "He"], empty)
  ]

instrReadTests = testGroup "Instructions READ tests"
  [ SC.testProperty "AccGlobal 0" $ (readInstruction $ pack [0x31]) == ((Just (AccGlobal 0)), empty)
  , SC.testProperty "Push" $ (readInstruction $ pack [0x4c]) == (Just (Push), empty)
  ]

hello = pack [
              0x4e, 0x45, 0x4b, 0x4f, 0x02, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x07, 0x00, 0x00, 0x00,
              0x03, 0x0d, 0x00, 0x48, 0x65, 0x6c, 0x6c, 0x6f, 0x20, 0x77, 0x6f, 0x72, 0x6c, 0x64, 0x21, 0x0a,
              0x01, 0x00, 0x70, 0x72, 0x69, 0x6e, 0x74, 0x00, 0x31, 0x4c, 0x2f, 0x2d, 0x58, 0x8b, 0xc8, 0xad
             ]

