module TIS100.Nodes.T30 where

import TIS100.Nodes.Base (Value (..))

newtype T30 = Stack [Value]
  deriving (Eq, Show)
