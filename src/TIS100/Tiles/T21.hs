module TIS100.Tiles.T21 where

import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Vector qualified as V
import TIS100.Tiles.Base (Port' (..), RunState (..), Value (..))
import TIS100.Tiles.ConnectedTile (IsConnectedTile (..))
import Prelude hiding (last)

data Register' = ACC | NIL | BAK
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

type TileProgram = V.Vector Instruction

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
  , tileProgram :: TileProgram
  }
  deriving (Eq, Show)

createTileState :: TileProgram -> T21
createTileState program_ =
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
    , tileProgram = program_
    }

getTileRunState :: T21 -> RunState
getTileRunState = runState . tileState

setTileRunState :: RunState -> T21 -> T21
setTileRunState rs tile = tile{tileState = (tileState tile){runState = rs}}

getPortVal :: Port' -> T21 -> (T21, Maybe Value)
getPortVal p t
  | p == ANY = error "Reads from ANY is not supported yet"
  | p == LAST = error "Reads from LAST is not supported yet"
  | p == LEFT = getPortVal' left t{tileState = (tileState t){left = Nothing, runState = rs}}
  | p == RIGHT = getPortVal' right t{tileState = (tileState t){right = Nothing, runState = rs}}
  | p == UP = getPortVal' up t{tileState = (tileState t){up = Nothing, runState = rs}}
  | p == DOWN = getPortVal' down t{tileState = (tileState t){down = Nothing, runState = rs}}
  | otherwise = error "Should not reach this code"
 where
  getPortVal' f t' = case f $ tileState t of
    Just v -> (t', Just v)
    Nothing -> (t{tileState = (tileState t){runState = WaitingOnRead p}}, Nothing)
  rs = if (runState . tileState) t == WaitingOnWrite p then Ready else (runState . tileState) t

setPortVal :: Port' -> Value -> T21 -> T21
setPortVal p v t
  | p == ANY = error "Writes to ANY is not supported yet"
  | p == LAST = error "Writes to LAST is not supported yet"
  | p == LEFT = t{tileState = (tileState t){left = Just v, runState = rs}}
  | p == RIGHT = t{tileState = (tileState t){right = Just v, runState = rs}}
  | p == UP = t{tileState = (tileState t){up = Just v, runState = rs}}
  | p == DOWN = t{tileState = (tileState t){down = Just v, runState = rs}}
  | otherwise = error "Should not reach this code"
 where
  rs = case (runState . tileState) t of
    WaitingOnRead _ -> Ready
    WaitingOnWrite p' -> WaitingOnWrite p'
    Ready -> WaitingOnWrite p

getRegVal :: Register' -> T21 -> Value
getRegVal r t = case r of
  ACC -> acc $ tileState t
  BAK -> bak $ tileState t
  NIL -> Value 0

setRegVal :: Register' -> Value -> T21 -> T21
setRegVal r v t = case r of
  ACC -> t{tileState = (tileState t){acc = v}}
  BAK -> t{tileState = (tileState t){bak = v}}
  NIL -> t

getCurrentInstruction :: T21 -> Maybe Instruction
getCurrentInstruction t = tileProgram t V.!? ix
 where
  (Address ix) = pc $ tileState t

incPC :: T21 -> T21
incPC t =
  if (runState $ tileState t) == Ready
    then t{tileState = (tileState t){pc = nextPC}}
    else t
 where
  (Address pc') = pc $ tileState t
  nextPC = Address $ (pc' + 1) `mod` V.length (tileProgram t)

addValueToPC :: (T21, Maybe Value) -> T21
addValueToPC (t, Just (Value v)) = t{tileState = (tileState t){pc = nextPC}}
 where
  (Address pc') = pc $ tileState t
  nextPC = Address $ (pc' + v) `mod` V.length (tileProgram t)
addValueToPC (t, Nothing) = t

instance IsConnectedTile T21 where
  getRunState = getTileRunState
  setRunState = setTileRunState

  readable p t = case p of
    ANY -> True
    LAST -> True
    LEFT -> isJust $ left $ tileState t
    RIGHT -> isJust $ right $ tileState t
    UP -> isJust $ up $ tileState t
    DOWN -> isJust $ down $ tileState t

  writable p t = case p of
    ANY -> True
    LAST -> True
    LEFT -> isNothing $ left $ tileState t
    RIGHT -> isNothing $ right $ tileState t
    UP -> isNothing $ up $ tileState t
    DOWN -> isNothing $ down $ tileState t

  isWaitingOnRead t = case getTileRunState t of
    WaitingOnRead p -> Just p
    _ -> Nothing

  isWaitingOnWrite t = case getTileRunState t of
    WaitingOnWrite p -> Just p
    _ -> Nothing

  readValueFrom = getPortVal

  writeValueTo = setPortVal

  step t_ = case (runState . tileState) t_ of
    Ready -> stepReady t_
    WaitingOnRead p_ -> stepWaitingOnRead t_ p_
    WaitingOnWrite p_ -> stepWaitingOnWrite t_ p_
   where
    stepWaitingOnWrite t _ = t

    stepWaitingOnRead :: T21 -> Port' -> T21
    stepWaitingOnRead t p = case getCurrentInstruction t of
      Nothing -> t
      Just (MOV (Port p') dst) ->
        if p == p'
          then case getPortVal p t of
            (t', Just v) -> incPC $ writeRegOrPort dst (t', Just v)
            (t', Nothing) -> t'
          else t
      _ -> t

    stepReady :: T21 -> T21
    stepReady t = fromMaybe t stepReady'
     where
      stepReady' :: Maybe T21
      stepReady' = case getCurrentInstruction t of
        Nothing -> Nothing
        Just NOP -> Just $ incPC t
        Just (MOVI v dst) -> Just $ incPC $ writeRegOrPort dst (t, Just v)
        Just (MOV src dst) -> Just $ incPC $ writeRegOrPort dst $ readRegOrPort src t
        Just SWP -> Just $ incPC $ swapAccBak t
        Just SAV -> Just $ incPC $ writeRegOrPort (Register BAK) $ readRegOrPort (Register ACC) t
        Just (ADDI v) -> Just $ incPC $ writeRegOrPort (Register ACC) $ maybeAddSub (+) (t, Just v) $ readRegOrPort (Register ACC) t
        Just (ADD src) -> Just $ incPC $ writeRegOrPort (Register ACC) $ maybeAddSub (+) (readRegOrPort src t) (readRegOrPort (Register ACC) t)
        Just (SUBI v) -> Just $ incPC $ writeRegOrPort (Register ACC) $ maybeAddSub (-) (t, Just v) $ readRegOrPort (Register ACC) t
        Just (SUB src) -> Just $ incPC $ writeRegOrPort (Register ACC) $ maybeAddSub (-) (readRegOrPort src t) (readRegOrPort (Register ACC) t)
        Just NEG -> Just $ incPC $ writeRegOrPort (Register ACC) $ maybeAddSub (-) (t, Just $ Value 0) $ readRegOrPort (Register ACC) t
        Just (JMP addr) -> Just $ t{tileState = (tileState t){pc = addr}}
        Just (JCC cond addr) -> case cond of
          EZ -> if acc (tileState t) == 0 then Just $ t{tileState = (tileState t){pc = addr}} else Just $ incPC t
          NZ -> if acc (tileState t) /= 0 then Just $ t{tileState = (tileState t){pc = addr}} else Just $ incPC t
          GZ -> if acc (tileState t) > 0 then Just $ t{tileState = (tileState t){pc = addr}} else Just $ incPC t
          LZ -> if acc (tileState t) < 0 then Just $ t{tileState = (tileState t){pc = addr}} else Just $ incPC t
        Just (JROI v) -> Just $ addValueToPC (t, Just v)
        Just (JRO src) -> Just $ addValueToPC $ readRegOrPort src t
      maybeAddSub :: (Value -> Value -> Value) -> (T21, Maybe Value) -> (T21, Maybe Value) -> (T21, Maybe Value)
      maybeAddSub f (t', Just v1) (_, Just v2) = (t', Just $ f v1 v2)
      maybeAddSub _ tv _ = tv -- Just to silence the linter

readRegOrPort :: RegisterOrPort -> T21 -> (T21, Maybe Value)
readRegOrPort rp t = case rp of
  Register r -> (t, Just v)
   where
    v = case r of
      ACC -> acc (tileState t)
      BAK -> bak (tileState t)
      NIL -> Value 0
  Port p -> getPortVal p t

writeRegOrPort :: RegisterOrPort -> (T21, Maybe Value) -> T21
writeRegOrPort rp (t, Just v) = case rp of
  Register r -> case r of
    ACC -> t{tileState = (tileState t){acc = v}}
    BAK -> t{tileState = (tileState t){bak = v}}
    NIL -> t
  Port p -> setPortVal p v t
writeRegOrPort _ (t, Nothing) = t

swapAccBak :: T21 -> T21
swapAccBak t =
  let accVal = getRegVal ACC t
      bakVal = getRegVal BAK t
      t' = setRegVal ACC bakVal t
      t'' = setRegVal BAK accVal t'
   in t''
