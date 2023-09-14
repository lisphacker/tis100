module TIS100.CPU.T21 where

import TIS100.CPU.Base (Value (..))
import Prelude hiding (last)

data Register' = ACC | NIL
  deriving (Eq, Show)

data Port' = ANY | LAST | LEFT | RIGHT | UP | DOWN
  deriving (Eq, Show)

data RegisterOrPort = Register Register' | Port Port'
  deriving (Eq, Show)

newtype Address = Address Int
  deriving (Eq, Show, Num)

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
  | JEZ Address
  | JNZ Address
  | JGZ Address
  | JLZ Address
  | JROI Value
  | JRO RegisterOrPort
  deriving (Eq, Show)

data RunState
  = Running
  | WaitingOnReadLeft
  | WaitingOnReadRight
  | WaitingOnReadUp
  | WaitingOnReadDown
  | WaitingOnReadAny
  | WaitingOnWriteLeft
  | WaitingOnWriteRight
  | WaitingOnWriteUp
  | WaitingOnWriteDown
  | WaitingOnWriteAny
  deriving (Eq, Show)

data TileState = TileState
  { acc :: Value,
    bak :: Value,
    left :: Maybe Value,
    right :: Maybe Value,
    up :: Maybe Value,
    down :: Maybe Value,
    last :: Port',
    program :: [Instruction],
    pc :: Address,
    runState :: RunState
  }
  deriving (Eq, Show)

createTileState :: [Instruction] -> TileState
createTileState program =
  TileState
    { acc = 0,
      bak = 0,
      left = Nothing,
      right = Nothing,
      up = Nothing,
      down = Nothing,
      last = UP,
      program = program,
      pc = 0,
      runState = Running
    }