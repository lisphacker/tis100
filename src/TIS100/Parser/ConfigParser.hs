module TIS100.Parser.ConfigParser where

import TIS100.Errors (TISErrorOr)

data NodeType = Conpute | Stack | Disabled
  deriving (Eq, Show)

data IOType = None | StdIO | List [Int] | File FilePath
  deriving (Eq, Show)

data Config = Config
  { rows :: Int,
    cols :: Int,
    nodes :: [[NodeType]],
    inputs :: [IOType],
    outputs :: [IOType]
  }
  deriving (Eq, Show)

parseConfig :: String -> TISErrorOr Config
parseConfig = undefined