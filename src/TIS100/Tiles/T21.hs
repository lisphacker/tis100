module TIS100.Tiles.T21 where

import Data.Maybe (fromMaybe)
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

getPortVal :: Bool -> Port' -> T21 -> (T21, Maybe Value)
getPortVal internalCall p t = if internalCall then getPortValInt else getPortValExt
 where
  getPortValInt = case rs of
    WaitingOnWrite _ _ -> (t, Nothing)
    WaitingOnRead _ Nothing -> (t, Nothing)
    WaitingOnRead p' (Just v) ->
      if p == p'
        then (t{tileState = (tileState t){runState = Ready}}, Just v)
        else (t, Nothing)
    Ready -> (t{tileState = (tileState t){runState = WaitingOnRead p Nothing}}, Nothing)

  getPortValExt = case rs of
    WaitingOnWrite p' v ->
      if p == p'
        then (incPC $ t{tileState = (tileState t){runState = Ready}}, Just v)
        else (t, Nothing)
    _ -> (t, Nothing)

  rs = runState . tileState $ t

setPortVal :: Bool -> Port' -> Value -> T21 -> Maybe T21
setPortVal internalCall p v t = if internalCall then setPortValInt else setPortValExt
 where
  setPortValInt = case rs of
    Ready -> Just $ t{tileState = (tileState t){runState = WaitingOnWrite p v}}
    _ -> Nothing

  setPortValExt = case rs of
    WaitingOnRead p' Nothing ->
      if p == p'
        then Just $ t{tileState = (tileState t){runState = WaitingOnRead p' (Just v)}}
        else Nothing
    _ -> Nothing

  rs = runState . tileState $ t

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
  if runState (tileState t) == Ready
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

  readValueFrom = getPortVal False -- External call

  writeValueTo = setPortVal False -- External call

  step t = case (runState . tileState) t of
    Ready -> stepReady
    WaitingOnRead _ Nothing -> t
    WaitingOnRead _ (Just _) -> stepReady
    WaitingOnWrite _ _ -> t
   where
    stepReady :: T21
    stepReady = fromMaybe t stepReady'
     where
      stepReady' :: Maybe T21
      stepReady' = case getCurrentInstruction t of
        Nothing -> Nothing
        Just NOP -> Just $ incPC t
        Just (MOVI v dst) -> Just $ incPC $ writeRegOrPort dst (t, Just v)
        Just (MOV src dst) -> Just $ incPC $ writeRegOrPort dst $ readRegOrPort src t
        Just SWP -> Just $ incPC $ swapAccBak t
        Just SAV -> Just $ incPC $ writeRegOrPort (Register BAK) $ readRegOrPort (Register ACC) t
        Just (ADDI v) -> Just $ incPC $ writeRegOrPort (Register ACC) $ maybeAddSub (flip (+)) (t, Just v) $ readRegOrPort (Register ACC) t
        Just (ADD src) -> Just $ incPC $ writeRegOrPort (Register ACC) $ maybeAddSub (flip (+)) (readRegOrPort src t) (readRegOrPort (Register ACC) t)
        Just (SUBI v) -> Just $ incPC $ writeRegOrPort (Register ACC) $ maybeAddSub (flip (-)) (t, Just v) $ readRegOrPort (Register ACC) t
        Just (SUB src) -> Just $ incPC $ writeRegOrPort (Register ACC) $ maybeAddSub (flip (-)) (readRegOrPort src t) (readRegOrPort (Register ACC) t)
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
  Port p -> getPortVal True p t

writeRegOrPort :: RegisterOrPort -> (T21, Maybe Value) -> T21
writeRegOrPort rp (t, Just v) = case rp of
  Register r -> case r of
    ACC -> t{tileState = (tileState t){acc = v}}
    BAK -> t{tileState = (tileState t){bak = v}}
    NIL -> t
  Port p -> fromMaybe t (setPortVal True p v t)
writeRegOrPort _ (t, Nothing) = t

swapAccBak :: T21 -> T21
swapAccBak t =
  let accVal = getRegVal ACC t
      bakVal = getRegVal BAK t
      t' = setRegVal ACC bakVal t
      t'' = setRegVal BAK accVal t'
   in t''
