module TIS100.Parser.Config where

import Data.IntMap qualified as IM

data NodeType = Conpute | Stack | Disabled
  deriving (Eq, Show)

data IOSource = StdIO | List [Int] | File FilePath
  deriving (Eq, Show)

type IODef = IM.IntMap IOSource

data Config = Config
  { rows :: Int
  , cols :: Int
  , nodes :: [[NodeType]]
  , inputs :: IODef
  , outputs :: IODef
  }
  deriving (Eq, Show)
