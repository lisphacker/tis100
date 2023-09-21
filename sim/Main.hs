{-# LANGUAGE QuasiQuotes #-}

module Main where

import CmdLine (CmdLineOpts (..), Config (..), parseCmdLine)
import TIS100.Parser.ConfigParser (parseConfig)

main :: IO ()
main = do
  cmdLineOpts <- parseCmdLine

  asmStr <- readFile $ asmFilePath cmdLineOpts
  cfgStr <- case config cmdLineOpts of
    Just (ConfigParamString cfgStr) -> return cfgStr
    Just (ConfigFileInput cfgFile) -> readFile cfgFile
    Nothing -> return ""

  cfg <- do
    case parseConfig cfgStr of
      Left err -> error $ show err
      Right cfg' -> return cfg'
  putStrLn $ show cfg

  return ()