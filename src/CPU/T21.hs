module CPU.T21 where

data Register' = ACC | NIL
  deriving (Eq, Show)

data Port' = ANY | LAST | LEFT | RIGHT | UP | DOWN
  deriving (Eq, Show)

data RegisterOrPort = Register Register' | Port Port'
  deriving (Eq, Show)

newtype Value = Value Int
  deriving (Eq, Show, Num)

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

data TileState = TileState
  { acc :: Value,
    bak :: Value,
    left :: Maybe Value,
    right :: Maybe Value,
    up :: Maybe Value,
    down :: Maybe Value,
    last :: Port',
    program :: [Instruction],
    pc :: Int
  }
  deriving (Eq, Show)