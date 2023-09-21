module TIS100.Nodes.T21 where

import TIS100.Nodes.Base (Value (..))
import Prelude hiding (last)

data Register' = ACC | NIL
  deriving (Eq, Show)

data Port' = ANY | LAST | LEFT | RIGHT | UP | DOWN
  deriving (Eq, Show)

data RegisterOrPort = Register Register' | Port Port'
  deriving (Eq, Show)

newtype Address = Address Int
  deriving (Eq, Show, Num)

data JumpCondition = EZ | NZ | GZ | LZ
  deriving (Eq, Show)

data Instruction
  = MOVI Value RegisterOrPort
  | MOV RegisterOrPort RegisterOrPort
  | SWP
  | SAV
  | ADDI Value
  | ADD RegisterOrPort
  | SUBI Value
  | SUB RegisterOrPort
  | NEG
  | JMP Address
  | JCC JumpCondition Address
  | JROI Value
  | JRO RegisterOrPort
  deriving (Eq, Show)

data RunState
  = Ready
  | WaitingOnRead Port'
  | WaitingOnWrite Port'
  deriving (Eq, Show)

data TileState = TileState
  { acc :: Value,
    bak :: Value,
    left :: Maybe Value,
    right :: Maybe Value,
    up :: Maybe Value,
    down :: Maybe Value,
    last :: Port',
    pc :: Address,
    runState :: RunState
  }
  deriving (Eq, Show)

data T21 = T21
  { tileState :: TileState,
    program :: [Instruction]
  }
  deriving (Eq, Show)

createTileState :: [Instruction] -> T21
createTileState program =
  T21
    { tileState =
        TileState
          { acc = 0,
            bak = 0,
            left = Nothing,
            right = Nothing,
            up = Nothing,
            down = Nothing,
            last = UP,
            pc = 0,
            runState = Ready
          },
      program = program
    }