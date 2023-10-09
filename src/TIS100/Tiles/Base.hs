module TIS100.Tiles.Base where

newtype Value = Value Int
  deriving (Eq)

instance Show Value where
  show (Value v) = show v

clamp :: Int -> Int
clamp = max (-999) . min 999

instance Num Value where
  (+) (Value a) (Value b) = Value $ clamp (a + b)
  (*) (Value a) (Value b) = Value $ clamp (a * b)
  (-) (Value a) (Value b) = Value $ clamp (a - b)
  abs (Value a) = Value $ abs a
  signum (Value a) = Value $ signum a
  fromInteger = Value . clamp . fromInteger

data RunState
  = Ready
  | WaitingOnRead Port'
  | WaitingOnWrite Port'
  deriving (Eq, Show)

data Port' = ANY | LAST | LEFT | RIGHT | UP | DOWN
  deriving (Eq, Show)

getOppositePort :: Port' -> Port'
getOppositePort ANY = ANY
getOppositePort LAST = LAST
getOppositePort LEFT = RIGHT
getOppositePort RIGHT = LEFT
getOppositePort UP = DOWN
getOppositePort DOWN = UP
