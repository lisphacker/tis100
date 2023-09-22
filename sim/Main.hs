{-# LANGUAGE QuasiQuotes #-}

module Main where

import CmdLine (CmdLineOpts (..), ConfigSource (..), parseCmdLine)
import TIS100.Parser.AsmParser (AsmSource, parseAsm)
import TIS100.Parser.ConfigParser (Config (..), parseConfig, readExternalInputs)

readConfig :: CmdLineOpts -> IO Config
readConfig cmdLineOpts = do
  cfgStr <- case config cmdLineOpts of
    ConfigParamString cfgStr -> return cfgStr
    ConfigFileInput cfgFile -> readFile cfgFile

  cfg <- do
    case parseConfig cfgStr of
      Left err -> error $ show err
      Right cfg' -> return cfg'
  cfg <- case config cmdLineOpts of
    ConfigFileInput cfgFile -> readExternalInputs cfgFile cfg
    _ -> readExternalInputs "" cfg
  return cfg

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
  putStrLn $ show asm

  return ()
