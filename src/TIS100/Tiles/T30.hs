module TIS100.Tiles.T30 where

import TIS100.Tiles.Base (Value (..))

newtype T30 = T30 [Value]
  deriving (Eq, Show)
