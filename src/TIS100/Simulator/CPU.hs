module TIS100.Simulator.CPU where

import Data.Vector.Mutable
import TIS100.Nodes.T21 (T21 (..))
import TIS100.Nodes.T30 (T30 (..))

data Node = InactiveNode | Node21 T21 | Node30 T30
  deriving (Eq, Show)

type CPUState m = MVector m Node