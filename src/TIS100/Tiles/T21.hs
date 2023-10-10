module TIS100.Tiles.T21 where

import Control.Applicative (Applicative (..))
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import Data.Vector qualified as V
import Data.Vector.Generic.New (run)
import GHC.Arr (ixmap)
import TIS100.Tiles.Base (Port' (..), RunState (..), Value (..))
import TIS100.Tiles.ConnectedTile (ConnectedTile (..), IsConnectedTile (..))
import Prelude hiding (last)

data Register' = ACC | NIL | BAK
  deriving (Eq, Show)

data RegisterOrPort = Register Register' | Port Port'
  deriving (Eq, Show)

newtype Address = Address Int
  deriving (Eq, Show, Num)

addValueToAddress :: Value -> Address -> Address
addValueToAddress (Value v) (Address a) = Address $ a + v
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

setTileRunState :: RunState -> T21 -> T21
setTileRunState rs tile = tile{tileState = (tileState tile){runState = rs}}

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

getCurrentInstruction :: T21 -> Maybe Instruction
getCurrentInstruction t = program t V.!? ix
 where
  (Address ix) = pc $ tileState t

incPC :: T21 -> T21
incPC t = t{tileState = (tileState t){pc = nextPC}}
 where
  (Address pc') = pc $ tileState t
  nextPC = Address $ (pc' + 1) `mod` V.length (program t)

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

  readValueFrom p t = (t', getPortVal p t)
   where
    t' = case p of
      ANY -> t
      LAST -> t
      LEFT -> t{tileState = (tileState t){left = Nothing, runState = Ready}}
      RIGHT -> t{tileState = (tileState t){right = Nothing, runState = Ready}}
      UP -> t{tileState = (tileState t){up = Nothing, runState = Ready}}
      DOWN -> t{tileState = (tileState t){down = Nothing, runState = Ready}}

  writeValueTo p v t = setPortVal p v t

  step t = case (runState . tileState) t of
    Ready -> stepReady t
    WaitingOnRead p -> stepWaitingOnRead t p
    WaitingOnWrite p -> stepWaitingOnWrite t p
   where
    stepWaitingOnWrite _ _ = t

    stepWaitingOnRead :: T21 -> Port' -> T21
    stepWaitingOnRead t p = case getCurrentInstruction t of
      Nothing -> t
      Just (MOV (Port p') dst) ->
        if p == p'
          then case getPortVal p t of
            Just v -> incPC $ setTileRunState Ready $ writeRegOrPort dst t v
            Nothing -> t
          else t
      _ -> t

    stepReady :: T21 -> T21
    stepReady t = fromMaybe t (stepReady' t)
     where
      stepReady' :: T21 -> Maybe T21
      stepReady' t = case getCurrentInstruction t of
        Nothing -> Nothing
        Just NOP -> Just $ incPC t
        Just (MOVI v dst) -> Just $ incPC $ writeRegOrPort dst t v
        Just (MOV src dst) -> incPC . writeRegOrPort dst t <$> readRegOrPort src t
        Just SWP -> Just $ incPC $ swapAccBak t
        Just SAV -> incPC . writeRegOrPort (Register BAK) t <$> readRegOrPort (Register ACC) t
        Just (ADDI v) -> incPC . writeRegOrPort (Register ACC) t . (+) v <$> readRegOrPort (Register ACC) t
        Just (ADD src) -> incPC . writeRegOrPort (Register ACC) t <$> liftA2 (+) (readRegOrPort src t) (readRegOrPort (Register ACC) t)
        Just (SUBI v) -> incPC . writeRegOrPort (Register ACC) t . (-) v <$> readRegOrPort (Register ACC) t
        Just (SUB src) -> incPC . writeRegOrPort (Register ACC) t <$> liftA2 (-) (readRegOrPort src t) (readRegOrPort (Register ACC) t)
        Just NEG -> incPC . writeRegOrPort (Register ACC) t . (-) (Value 0) <$> readRegOrPort (Register ACC) t
        Just (JMP addr) -> Just $ t{tileState = (tileState t){pc = addr}}
        Just (JCC cond addr) -> case cond of
          EZ -> if acc (tileState t) == 0 then Just $ t{tileState = (tileState t){pc = addr}} else Just $ incPC t
          NZ -> if acc (tileState t) /= 0 then Just $ t{tileState = (tileState t){pc = addr}} else Just $ incPC t
          GZ -> if acc (tileState t) > 0 then Just $ t{tileState = (tileState t){pc = addr}} else Just $ incPC t
          LZ -> if acc (tileState t) < 0 then Just $ t{tileState = (tileState t){pc = addr}} else Just $ incPC t
        Just (JROI v) -> Just $ t{tileState = (tileState t){pc = addValueToAddress v (pc $ tileState t)}}
        Just (JRO src) -> incPC . (\v -> t{tileState = (tileState t){pc = addValueToAddress v (pc $ tileState t)}}) <$> readRegOrPort src t

    readRegOrPort :: RegisterOrPort -> T21 -> Maybe Value
    readRegOrPort rp t = case rp of
      Register r -> Just $ case r of
        ACC -> acc (tileState t)
        BAK -> bak (tileState t)
        NIL -> Value 0
      Port p -> getPortVal p t

    writeRegOrPort :: RegisterOrPort -> T21 -> Value -> T21
    writeRegOrPort rp t v = case rp of
      Register r -> case r of
        ACC -> t{tileState = (tileState t){acc = v}}
        BAK -> t{tileState = (tileState t){bak = v}}
        NIL -> t
      Port p -> setPortVal p v t

    swapAccBak :: T21 -> T21
    swapAccBak t =
      let acc = fromJust $ readRegOrPort (Register ACC) t
          bak = fromJust $ readRegOrPort (Register BAK) t
       in (flip $ writeRegOrPort (Register ACC)) bak $ writeRegOrPort (Register BAK) t acc
