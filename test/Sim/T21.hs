module Sim.T21 where

import Control.Monad (forM_)
import Data.Vector qualified as V
import TIS100.Tiles.Base
import TIS100.Tiles.ConnectedTile (step)
import TIS100.Tiles.T21
import Test.Hspec
import Prelude hiding (init, last)

mkT21TileWithPC :: Address -> Value -> Value -> [Instruction] -> T21
mkT21TileWithPC initPc initAcc initBak instns =
  T21
    { tileProgram =
        V.fromList instns
    , tileState =
        TileState
          { acc = initAcc
          , bak = initBak
          , last = ANY
          , pc = initPc
          , runState = Ready
          }
    }

mkT21Tile :: Value -> Value -> [Instruction] -> T21
mkT21Tile = mkT21TileWithPC 0

testADDSUB :: Bool -> Spec
testADDSUB add = describe ("Testing " ++ insName) $ do
  testADDSUB_ACC
  testADDSUB_BAK
  forM_ [UP, DOWN, LEFT, RIGHT] testADDSUB_Port
 where
  testADDSUB_ACC = do
    let init = mkT21Tile 10 20 [if add then ADD (Register ACC) else SUB (Register ACC)]
    let next = step init

    describe ("Testing " ++ insName ++ " ACC") $ do
      it "Status" $ do
        acc (tileState next) `shouldBe` Value (f 10 10)

  testADDSUB_BAK = do
    let init = mkT21Tile 10 20 [if add then ADD (Register BAK) else SUB (Register BAK)]
    let next = step init

    describe ("Testing " ++ insName ++ " ACC") $ do
      it "Status" $ do
        acc (tileState next) `shouldBe` Value (f 10 20)

  testADDSUB_Port port = do
    let init = mkT21Tile 10 20 [if add then ADD (Port port) else SUB (Port port)]
    let next = step init

    describe ("Testing " ++ insName ++ " " ++ show port) $ do
      it "Status" $ do
        acc (tileState next) `shouldBe` Value 10
        runState (tileState next) `shouldBe` WaitingOnRead port Nothing

  f = if add then (+) else (-)
  insName = if add then "ADD" else "SUB"

testADD :: Spec
testADD = testADDSUB True
testSUB :: Spec
testSUB = testADDSUB False

testADDSUBI :: Bool -> Spec
testADDSUBI add = do
  let init = mkT21Tile 10 0 [if add then ADDI (Value 20) else SUBI (Value 20)]
  let next = step init

  describe ("Testing " ++ insName) $ do
    it "Status" $ do
      acc (tileState next) `shouldBe` (f (Value 10) (Value 20))
 where
  f = if add then (+) else (-)
  insName = if add then "ADDI" else "SUBI"

testADDI :: Spec
testADDI = testADDSUBI True
testSUBI :: Spec
testSUBI = testADDSUBI False

testUnconditionalJump :: Spec
testUnconditionalJump = describe "Testing JMP" $ do
  testUnconditionalJump' "without overflow/underflow" (JMP (Address 5)) (Address 5)
  testUnconditionalJump' "underflow" (JMP (Address (-5))) (Address 0)
  testUnconditionalJump' "overflow" (JMP (Address 10)) (Address 6)
 where
  testUnconditionalJump' desc ins tgtAddr = do
    let next = step $ init ins

    describe ("Testing " ++ desc) $ do
      it "Status" $ do
        pc (tileState next) `shouldBe` tgtAddr

  init ins = mkT21TileWithPC 3 2 0 [NOP, NOP, NOP, ins, NOP, NOP, NOP]

testJEZ :: Spec
testJEZ = undefined
testJGZ :: Spec
testJGZ = undefined
testJLZ :: Spec
testJLZ = undefined
testJNZ :: Spec
testJNZ = undefined
testJMP :: Spec
testJMP = testUnconditionalJump
testJRO :: Spec
testJRO = undefined

testMOVI :: Spec
testMOVI = describe "Testing MOVI" $ do
  let ports = [UP, DOWN, LEFT, RIGHT]
  testMOVI_ACC
  forM_ ports testMOVI_Port
 where
  testMOVI_ACC = do
    let init = mkT21Tile 10 20 [MOVI (Value 40) (Register ACC)]
    let next = step init

    describe "Testing MOVI <val>, ACC" $ do
      it "Status" $ do
        acc (tileState next) `shouldBe` Value 40

  testMOVI_Port port = do
    let init = mkT21Tile 10 20 [MOVI (Value 40) (Port port)]
    let next = step init

    describe ("Testing MOVI <val>, " ++ show port) $ do
      it "Status" $ do
        runState (tileState next) `shouldBe` WaitingOnWrite port (Value 40)

testMOV :: Spec
testMOV = describe "Testing MOV" $ do
  let ports = [UP, DOWN, LEFT, RIGHT]
  forM_ ports testMOV_ACC_Port
  forM_ ports testMOV_Port_ACC
 where
  testMOV_ACC_Port port = do
    let init = mkT21Tile 10 20 [MOV (Register ACC) (Port port)]
    let next = step init

    describe ("Testing MOV ACC, " ++ show port) $ do
      it "Status" $ do
        runState (tileState next) `shouldBe` WaitingOnWrite port (acc $ tileState init)

  testMOV_Port_ACC port = do
    let init = mkT21Tile 10 20 [MOV (Port port) (Register ACC)]
    let next = step init

    describe ("Testing MOV " ++ show port ++ ", ACC") $ do
      it "Status" $ do
        runState (tileState next) `shouldBe` WaitingOnRead port Nothing

testNEG :: Spec
testNEG = describe "Testing NEG" $ do
  let init = mkT21Tile 10 20 [NEG]
  let next = step init

  describe "Testing NEG" $ do
    it "Status" $ do
      acc (tileState next) `shouldBe` Value (-10)

testNOP :: Spec
testNOP = describe "Testing NOPs" $ do
  testNOP' NOP
  testNOP' (ADD (Register NIL))
  testNOP' (SUB (Register NIL))
 where
  testNOP' ins = do
    let init = mkT21Tile 10 20 [ins]
    let next = step init

    describe ("Testing " ++ show ins) $ do
      it "Status" $ do
        next `shouldBe` init

testSWP :: Spec
testSWP = describe "Testing SWP" $ do
  let init = mkT21Tile 10 20 [SWP]
  let next = step init

  describe "Testing SWP" $ do
    it "Status" $ do
      acc (tileState next) `shouldBe` Value 20
      bak (tileState next) `shouldBe` Value 10

simTestsSpec :: Spec
simTestsSpec = describe "Intra-T21 tests" $ parallel $ do
  testADD
  testADDI
  -- testJEZ
  -- testJGZ
  -- testJLZ
  -- testJNZ
  testJMP
  -- testJRO
  testMOV
  testMOVI
  testNEG
  testNOP
  testSUB
  testSUBI
  testSWP
