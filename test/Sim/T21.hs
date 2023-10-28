module Sim.T21 where

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

testADDI :: Spec
testADDI = do
  let init = mkT21Tile 10 0 [ADDI (Value 20)]
  let next = step init

  describe "Testing ADDI" $ do
    it "Status" $ do
      acc (tileState next) `shouldBe` Value 30

testMOVI :: Spec
testMOVI = do
  let init = mkT21Tile 10 20 [MOVI (Value 40) (Register ACC)]
  let next = step init

  describe "Testing MOVI" $ do
    it "Status" $ do
      acc (tileState next) `shouldBe` Value 40

testNOP :: Spec
testNOP = do
  let init = mkT21Tile 10 20 [NOP]
  let next = step init

  describe "Testing NOP" $ do
    it "Status" $ do
      next `shouldBe` init

simTestsSpec :: Spec
simTestsSpec = describe "Intra-T21 tests" $ parallel $ do
  testADDI
  testMOVI
  testNOP
