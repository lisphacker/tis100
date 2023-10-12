module CmdLine where

import TIS100.Sim.Config (ConfigSource (..), SimRunConfig (..))

import Options.Applicative

cfgFileInput :: Parser ConfigSource
cfgFileInput =
  ConfigFileInput
    <$> strOption
      ( long "config"
          <> short 'c'
          <> metavar "CONFIG_FILE"
          <> help "Config file"
      )

cfgStringInput :: Parser ConfigSource
cfgStringInput =
  ConfigParamString
    <$> strOption
      ( long "config-str"
          <> short 's'
          <> metavar "CONFIG_STRING"
          <> help "Config string"
      )

sample :: Parser SimRunConfig
sample =
  SimRunConfig
    <$> strArgument
      ( metavar "ASM_FILE"
          <> help "Assembly file"
      )
    <*> (cfgFileInput <|> cfgStringInput)

parseCmdLine :: IO SimRunConfig
parseCmdLine = execParser opts
 where
  opts =
    info
      (sample <**> helper)
      ( fullDesc
          <> progDesc "Runs a TIS-100 asssembly program on a simuated TIS-100 machine."
          <> header "tissim - A TIS-100 simulator"
      )
