module TIS100.Parser.Config where

import Data.IntMap qualified as IM

data TileType = Conpute | Stack | Disabled
  deriving (Eq, Show)

data IOSource = StdIO | List [Int] | File FilePath
  deriving (Eq, Show)

type IODef = IM.IntMap IOSource

data Config = Config
  { rows :: Int
  , cols :: Int
  , tiles :: [[TileType]]
  , inputs :: IODef
  , outputs :: IODef
  , refOutputs :: IODef
  }
  deriving (Eq, Show)
