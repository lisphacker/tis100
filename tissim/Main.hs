module Main where

import CmdLine (parseCmdLine)
import Control.Monad (foldM, replicateM, void)
import Data.Either (fromRight)
import Data.IntMap qualified as IM
import Data.Vector qualified as V
import TIS100.Parser.AsmParser (AsmSource, parseAsm)
import TIS100.Parser.Config qualified as ParserCfg
import TIS100.Parser.ConfigParser (parseConfig, readExternalInputs)
import TIS100.Parser.Util qualified as ParserUtil
import TIS100.Sim.CPU qualified as CPU
import TIS100.Sim.Config (ConfigSource (..), SimRunConfig (..))
import TIS100.Sim.Run qualified as Run

main :: IO ()
main = do
  cmdLineOpts <- parseCmdLine

  cfg <- ParserUtil.readConfig cmdLineOpts
  print cfg

  asm <- ParserUtil.readAsm cmdLineOpts
  print asm

  let initialCPUState = CPU.createInitialCPUState cfg asm
  finalSimState <- case initialCPUState of
    Left err -> error $ show err
    Right cpuState -> Run.run $ Run.SimState cpuState (ParserCfg.inputs cfg) (ParserCfg.outputs cfg)

  print ""
  print "Final state"
  print finalSimState
  print "Ref Output"
  print $ show $ ParserCfg.refOutputs cfg
  print "Test Output"
  print $ show $ Run.outputs finalSimState
  return ()
