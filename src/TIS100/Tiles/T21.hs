module TIS100.Tiles.T21 where

import TIS100.Parser.AsmParser (LabelOrInstruction (NOP))
import TIS100.Tiles.Base (Port' (..), Value (..))
import TIS100.Tiles.ConnectedTile (ConnectedTile (..))
import Prelude hiding (last)

data Register' = ACC | NIL
  deriving (Eq, Show)

data RegisterOrPort = Register Register' | Port Port'
  deriving (Eq, Show)

newtype Address = Address Int
  deriving (Eq, Show, Num)

data JumpCondition = EZ | NZ | GZ | LZ
  deriving (Eq, Show)

data Instruction
  = NOP
  | MOVI Value RegisterOrPort
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

type TileProgram = [Instruction]

data RunState
  = Ready
  | WaitingOnRead Port'
  | WaitingOnWrite Port'
  deriving (Eq, Show)

data TileState = TileState
  { acc :: Value
  , bak :: Value
  , left :: Maybe Value
  , right :: Maybe Value
  , up :: Maybe Value
  , down :: Maybe Value
  , last :: Port'
  , pc :: Address
  , runState :: RunState
  }
  deriving (Eq, Show)

data T21 = T21
  { tileState :: TileState
  , program :: TileProgram
  }
  deriving (Eq, Show)

createTileState :: TileProgram -> T21
createTileState program =
  T21
    { tileState =
        TileState
          { acc = 0
          , bak = 0
          , left = Nothing
          , right = Nothing
          , up = Nothing
          , down = Nothing
          , last = UP
          , pc = 0
          , runState = Ready
          }
    , program = program
    }

getTileRunState :: T21 -> RunState
getTileRunState = runState . tileState

setTileRunState :: T21 -> RunState -> T21
setTileRunState tile rs = tile{tileState = (tileState tile){runState = rs}}

getOppositePort :: Port' -> Port'
getOppositePort ANY = ANY
getOppositePort LAST = LAST
getOppositePort LEFT = RIGHT
getOppositePort RIGHT = LEFT
getOppositePort UP = DOWN
getOppositePort DOWN = UP

getPortVal :: Port' -> T21 -> Maybe Value
-- getPortVal ANY t = getPortVal (last t) t
-- getPortVal LAST t = getPortVal (last t) t
getPortVal LEFT t = left $ tileState t
getPortVal RIGHT t = right $ tileState t
getPortVal UP t = up $ tileState t
getPortVal DOWN t = down $ tileState t

setPortVal :: Port' -> Value -> T21 -> T21
-- setPortVal ANY v t = setPortVal (last t) v t
-- setPortVal LAST v t = setPortVal (last t) v t
setPortVal LEFT v t = t{tileState = (tileState t){left = Just v}}
setPortVal RIGHT v t = t{tileState = (tileState t){right = Just v}}
setPortVal UP v t = t{tileState = (tileState t){up = Just v}}
setPortVal DOWN v t = t{tileState = (tileState t){down = Just v}}

clearPortVal :: Port' -> Value -> T21 -> T21
-- clearPortVal ANY v t = clearPortVal (last t) v t
-- clearPortVal LAST v t = clearPortVal (last t) v t
clearPortVal LEFT v t = t{tileState = (tileState t){left = Nothing}}
clearPortVal RIGHT v t = t{tileState = (tileState t){right = Nothing}}
clearPortVal UP v t = t{tileState = (tileState t){up = Nothing}}
clearPortVal DOWN v t = t{tileState = (tileState t){down = Nothing}}

instance ConnectedTile T21 where
  readValueFrom t p = (t, getPortVal p t)
  writeValueTo t p v = setPortVal p v t
  step t = t