module CmdLine where

import Options.Applicative

data Config
  = ConfigcfgFileInput FilePath
  | ConfigParamString String
  deriving (Show)

data CmdLineOpts = CmdLineOpts
  { asmFilePath :: String,
    config :: Maybe Config
  }
  deriving (Show)

cfgFileInput :: Parser Config
cfgFileInput =
  ConfigcfgFileInput
    <$> strOption
      ( long "config"
          <> short 'c'
          <> metavar "CONFIG_FILE"
          <> help "Config file"
      )

cfgcfgStringInput :: Parser Config
cfgcfgStringInput =
  ConfigParamString
    <$> strOption
      ( long "config-str"
          <> metavar "CONFIG_STRING"
          <> help "Config string"
      )

sample :: Parser CmdLineOpts
sample =
  CmdLineOpts
    <$> strArgument
      ( metavar "ASM_FILE"
          <> help "Assembly file"
      )
    <*> optional (cfgFileInput <|> cfgcfgStringInput)

parseCmdLine :: IO CmdLineOpts
parseCmdLine = execParser opts
  where
    opts =
      info
        (sample <**> helper)
        ( fullDesc
            <> progDesc "Runs a TIS-100 asssembly program on a simuated TIS-100 machine."
            <> header "tissim - A TIS-100 simulator"
        )
