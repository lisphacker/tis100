module Main where

import CmdLine (parseCmdLine)
import TIS100.Parser.Config qualified as ParserCfg
import TIS100.Parser.Util qualified as ParserUtil
import TIS100.Sim.CPU qualified as CPU
import TIS100.Sim.Run qualified as Run

main :: IO ()
main = do
  cmdLineOpts <- parseCmdLine

  cfg <- ParserUtil.readConfig cmdLineOpts
  print cfg

  asm <- ParserUtil.readAsm cmdLineOpts
  print asm

  let initialCPUState = CPU.createInitialCPUState cfg asm
  print initialCPUState

  finalSimState <- case initialCPUState of
    Left err -> error $ show err
    Right cpuState -> Run.run $ Run.SimState cpuState (ParserCfg.inputs cfg) (ParserCfg.outputs cfg)

  putStrLn ""
  putStrLn "Final state"
  print finalSimState
  putStrLn "Ref Output"
  print $ ParserCfg.refOutputs cfg
  putStrLn "Test Output"
  print $ Run.outputs finalSimState
  return ()
