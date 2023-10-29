module Sim.T21 where

import Control.Monad (forM_)
import Data.Vector qualified as V
import TIS100.Tiles.Base
import TIS100.Tiles.ConnectedTile (step)
import TIS100.Tiles.T21
import Test.Hspec
import Prelude hiding (last)

mkT21Tile :: Value -> Value -> [Instruction] -> T21
mkT21Tile initAcc initBak instns =
  T21
    { tileProgram =
        V.fromList instns
    , tileState =
        TileState
          { acc = initAcc
          , bak = initBak
          , last = ANY
          , pc = 0
          , runState = Ready
          }
    }

testADD :: Bool -> Spec
testADD add = describe ("Testing " ++ insName) $ do
  testADD_ACC
  testADD_BAK
  forM_ [UP, DOWN, LEFT, RIGHT] testADD_Port
 where
  testADD_ACC = do
    let init = mkT21Tile 10 20 [if add then ADD (Register ACC) else SUB (Register ACC)]
    let next = step init

    describe ("Testing " ++ insName ++ " ACC") $ do
      it "Status" $ do
        acc (tileState next) `shouldBe` Value (f 10 10)

  testADD_BAK = do
    let init = mkT21Tile 10 20 [if add then ADD (Register BAK) else SUB (Register BAK)]
    let next = step init

    describe ("Testing " ++ insName ++ " ACC") $ do
      it "Status" $ do
        acc (tileState next) `shouldBe` Value (f 10 20)

  testADD_Port port = do
    let init = mkT21Tile 10 20 [if add then ADD (Port port) else SUB (Port port)]
    let next = step init

    describe ("Testing " ++ insName ++ " " ++ show port) $ do
      it "Status" $ do
        acc (tileState next) `shouldBe` Value 10
        runState (tileState next) `shouldBe` WaitingOnRead port Nothing

  f = if add then (+) else (-)
  insName = if add then "ADD" else "SUB"

testADDI :: Spec
testADDI = do
  let init = mkT21Tile 10 0 [ADDI (Value 20)]
  let next = step init

  describe "Testing ADDI" $ do
    it "Status" $ do
      acc (tileState next) `shouldBe` Value 30

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

simTestsSpec :: Spec
simTestsSpec = describe "Intra-T21 tests" $ parallel $ do
  testADD True
  testADD False
  testADDI
  testMOV
  testMOVI
  testNOP
