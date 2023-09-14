module TIS100.CPU.T30 where

import TIS100.CPU.Base (Value (..))

newtype Stack = Stack [Value]
  deriving (Eq, Show)