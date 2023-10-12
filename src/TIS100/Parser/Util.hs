module TIS100.Parser.Util where

import TIS100.Parser.AsmParser (AsmSource, parseAsm)
import TIS100.Parser.Config (Config (..))
import TIS100.Parser.ConfigParser (parseConfig, readExternalInputs)
import TIS100.Sim.Config (ConfigSource (..), SimRunConfig (..))

readConfig :: SimRunConfig -> IO Config
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

readAsm :: SimRunConfig -> IO AsmSource
readAsm cmdLineOpts = do
  asmStr <- readFile $ asmFilePath cmdLineOpts

  case parseAsm asmStr of
    Left err -> error $ show err
    Right asm -> return asm
