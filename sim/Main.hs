module Main where

import CmdLine (CmdLineOpts (..), ConfigSource (..), parseCmdLine)
import TIS100.Nodes.Config (Config)
import TIS100.Parser.AsmParser (AsmSource, parseAsm)
import TIS100.Parser.ConfigParser (parseConfig, readExternalInputs)

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
  -- putStrLn $ show cfg

  asm <- readAsm cmdLineOpts
  print asm

  return ()
