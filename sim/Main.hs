module Main where

import CmdLine (CmdLineOpts (..), ConfigSource (..), parseCmdLine)
import Control.Monad (foldM, replicateM, void)
import Data.Either (fromRight)
import Data.IntMap qualified as IM
import Data.Vector qualified as V
import TIS100.Parser.AsmParser (AsmSource, parseAsm)
import TIS100.Parser.Config (Config (..))
import TIS100.Parser.ConfigParser (parseConfig, readExternalInputs)
import TIS100.Sim.CPU qualified as CPU
import TIS100.Sim.Run qualified as Run

readConfig :: CmdLineOpts -> IO Config
readConfig cmdLineOpts = do
  cfgStr <- case config cmdLineOpts of
    ConfigParamString cfgStr -> return cfgStr
    ConfigFileInput cfgFile -> readFile cfgFile

  cfg <- do
    case parseConfig cfgStr of
      Left err -> error $ show err
      Right cfg' -> return cfg'
  case config cmdLineOpts of
    ConfigFileInput cfgFile -> readExternalInputs cfgFile cfg
    _ -> readExternalInputs "" cfg

readAsm :: CmdLineOpts -> IO AsmSource
readAsm cmdLineOpts = do
  asmStr <- readFile $ asmFilePath cmdLineOpts

  case parseAsm asmStr of
    Left err -> error $ show err
    Right asm -> return asm

loopUntilNoChange :: Int -> Run.SimState -> IO Run.SimState
loopUntilNoChange i s = do
  nextSimState <- Run.runStep s
  -- print $ "Iteration " ++ show i
  -- print $ "Before: "
  -- print $ "  " ++ show (V.head . CPU.tiles . Run.cpu $ s)
  -- print $ "  " ++ show (((flip (V.!)) 4) . CPU.tiles . Run.cpu $ s)
  -- print $ "  " ++ show (((flip (V.!)) 8) . CPU.tiles . Run.cpu $ s)
  -- print $ "  IN:  " ++ show (IM.lookup 0 $ Run.inputs s)
  -- print $ "  OUT: " ++ show (IM.lookup 0 $ Run.outputs s)
  -- print $ "After:  "
  -- print $ "  " ++ show (V.head . CPU.tiles . Run.cpu $ nextSimState)
  -- print $ "  " ++ show (((flip (V.!)) 4) . CPU.tiles . Run.cpu $ nextSimState)
  -- print $ "  " ++ show (((flip (V.!)) 8) . CPU.tiles . Run.cpu $ nextSimState)
  -- print $ "  IN:  " ++ show (IM.lookup 0 $ Run.inputs nextSimState)
  -- print $ "  OUT: " ++ show (IM.lookup 0 $ Run.outputs nextSimState)
  if nextSimState == s
    then return s
    else loopUntilNoChange (i + 1) nextSimState

main :: IO ()
main = do
  cmdLineOpts <- parseCmdLine

  cfg <- readConfig cmdLineOpts
  print cfg

  asm <- readAsm cmdLineOpts
  print asm

  let initialCPUState = CPU.createInitialCPUState cfg asm
  finalSimState <- case initialCPUState of
    Left err -> error $ show err
    Right cpuState -> loopUntilNoChange 1 $ Run.SimState cpuState (inputs cfg) (outputs cfg)

  print ""
  print "Final state"
  print finalSimState
  print "Ref Output"
  print $ show $ refOutputs cfg
  print "Test Output"
  print $ show $ Run.outputs finalSimState
  return ()
