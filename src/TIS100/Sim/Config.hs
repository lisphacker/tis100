module TIS100.Sim.Config where

data ConfigSource
  = ConfigFileInput FilePath
  | ConfigParamString String
  deriving (Show)

data SimRunConfig = SimRunConfig
  { asmFilePath :: FilePath
  , config :: ConfigSource
  }
  deriving (Show)
