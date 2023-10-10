module Main where

import CmdLine (CmdLineOpts (..), ConfigSource (..), parseCmdLine)
import Control.Monad (foldM, replicateM)
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

runStep :: Run.SimState -> Int -> IO Run.SimState
runStep s i = do
  nextSimState <- Run.runStep s
  print ""
  print $ "Iteration " ++ show i
  print $ "Before: " ++ show (V.head . CPU.tiles . Run.cpu $ s) ++ " " ++ show (IM.lookup 0 $ Run.inputs s)
  print $ "After:  " ++ show (V.head . CPU.tiles . Run.cpu $ nextSimState) ++ " " ++ show (IM.lookup 0 $ Run.inputs s)
  return nextSimState

main :: IO ()
main = do
  cmdLineOpts <- parseCmdLine

  cfg <- readConfig cmdLineOpts
  print cfg

  asm <- readAsm cmdLineOpts
  print asm

  let initialCPUState = CPU.createInitialCPUState cfg asm

  print ""
  -- print initialCPUState
  print $ V.head . CPU.tiles <$> initialCPUState

  nextSimState <- case initialCPUState of
    Left err -> error $ show err
    -- Right cpuState -> Run.SimState cpuState (inputs cfg) (outputs cfg) >>= (replicateM 20 . Run.runStep)
    Right cpuState -> foldM runStep (Run.SimState cpuState (inputs cfg) (outputs cfg)) [1 .. 20]

  print ""
  print $ V.head . CPU.tiles . Run.cpu $ nextSimState
  -- print $ V.head . CPU.tiles . Run.cpu $ nextSimStates !! 19

  return ()
