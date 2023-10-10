module Main where

import CmdLine (CmdLineOpts (..), ConfigSource (..), parseCmdLine)
import Control.Monad (foldM, replicateM)
import Data.Either (fromRight)
import qualified Data.Vector as V
import TIS100.Parser.AsmParser (AsmSource, parseAsm)
import TIS100.Parser.Config (Config (..))
import TIS100.Parser.ConfigParser (parseConfig, readExternalInputs)
import qualified TIS100.Sim.CPU as CPU
import qualified TIS100.Sim.Run as Run

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
    Right cpuState -> foldM (\s i -> Run.runStep s) (Run.SimState cpuState (inputs cfg) (outputs cfg)) [1 .. 20]

  print ""
  print $ V.head . CPU.tiles . Run.cpu $ nextSimState
  -- print $ V.head . CPU.tiles . Run.cpu $ nextSimStates !! 19

  return ()
