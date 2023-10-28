module Sim.Examples where

import GHC.IO (unsafePerformIO)
import TIS100.Parser.Config qualified as ParserCfg
import TIS100.Parser.Util qualified as ParserUtil
import TIS100.Sim.CPU qualified as CPU
import TIS100.Sim.Config (ConfigSource (..), SimRunConfig (SimRunConfig))
import TIS100.Sim.Run qualified as Run
import Test.Hspec

testExampleAsm :: String -> FilePath -> FilePath -> Spec
testExampleAsm n asmFilePath cfgFilePath = do
  let (ref, test) = unsafePerformIO $ runSim

  describe ("Testing " ++ n) $ do
    it "Status" $ do
      test `shouldBe` ref
 where
  runSim :: IO (ParserCfg.IODef, ParserCfg.IODef)
  runSim = do
    let simRunCfg = SimRunConfig asmFilePath (ConfigFileInput cfgFilePath)
    cfg <- ParserUtil.readConfig simRunCfg
    asm <- ParserUtil.readAsm simRunCfg

    let initialCPUState = CPU.createInitialCPUState cfg asm
    finalSimState <- case initialCPUState of
      Left err -> error $ show err
      Right cpuState -> Run.run $ Run.SimState cpuState (ParserCfg.inputs cfg) (ParserCfg.outputs cfg)

    return (ParserCfg.refOutputs cfg, Run.outputs finalSimState)

simTestsSpec :: Spec
simTestsSpec = describe "Example Tests" $ parallel $ do
  testExampleAsm "segment00150" "examples/segment00150/segment00150.asm" "examples/segment00150/segment00150.cfg"
  testExampleAsm "segment20176" "examples/segment20176/segment20176.asm" "examples/segment20176/segment20176.cfg"
