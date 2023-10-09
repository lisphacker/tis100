module TIS100.Sim.Run where

import Control.Monad (foldM)
import Control.Monad.ST
import Data.Maybe (fromJust, fromMaybe)
import Data.Vector qualified as MV
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import Foreign qualified as V
import TIS100.Parser.Config (IODef)
import TIS100.Parser.Config qualified as CFG
import TIS100.Sim.CPU qualified as CPU
import TIS100.Tiles.Base qualified as Tiles
import TIS100.Tiles.ConnectedTile (ConnectedTile (..), IsConnectedTile (..))
import TIS100.Tiles.Inactive qualified as Inactive
import TIS100.Tiles.T21 qualified as T21
import TIS100.Tiles.T30 qualified as T30

data SimState = SimState
  { cpu :: CPU.CPUState
  , inputs :: CFG.IODef
  , outputs :: CFG.IODef
  }
  deriving (Show)

type RWTileVector = MV.MVector RealWorld CPU.PositionedTile

{-
class (Show t) => IsConnectedTile t where
  processTileComm :: t -> (Int, Int) -> (Int, Int) -> (RWTileVector, IODef, IODef) -> IO (RWTileVector, IODef, IODef)

data ConnectedTile
  = forall t.
    (IsConnectedTile t) =>
    ConnectedTile t

instance IsConnectedTile T21 where
  processTileComm t (r, c, i) (rows, cols) (tiles, ins, outs) = do
    return $ case (T21.tileState . T21.runState) t of
      T21.WaitingOnRead p -> case p of
        LEFT ->
          if c == 0
            then (tiles, ins, outs)
            else do
              let o = i - 1
              otile <- MV.read tiles o

instance IsConnectedTile T30 where
  processTileComm = undefined

instance IsConnectedTile InactiveTile where
  processTileComm = undefined

instance Show ConnectedTile where
  show (ConnectedTile t) = show t

type TileM s = ST s (MV.MVector s CPU.Tile)
-}

step :: SimState -> IO SimState
step = processTiles >> processComm

processComm :: SimState -> IO SimState
processComm (SimState (CPU.CPUState (CPU.CPUConfig rows cols) tiles) ins outs) = do
  mtiles <- V.thaw tiles
  let nTiles = rows * cols
  (mtiles', ins', outs') <- foldM processTileComm' (mtiles, ins, outs) [0 .. nTiles - 1]
  tiles' <- V.freeze mtiles'
  return $ SimState (CPU.CPUState (CPU.CPUConfig rows cols) tiles') ins' outs'
 where
  processTileComm' :: (RWTileVector, IODef, IODef) -> Int -> IO (RWTileVector, IODef, IODef)
  processTileComm' (tiles, ins, outs) i = do
    ptile <- MV.read tiles i
    let tile = CPU.tile ptile
    let (r, c) = CPU.pos ptile

    case getRunState tile of
      Tiles.WaitingOnRead p -> do
        if r == 0
          then return (tiles, ins, outs)
          else do
            let o = getOtherTile i p
            optile <- MV.read tiles o
            let otile = CPU.tile optile
            let op = Tiles.getOppositePort p
            if readable otile op
              then do
                let (tile', val) = readValueFrom tile p
                let otile' = writeValueTo otile op $ fromJust val
                MV.write tiles i $ ptile{CPU.tile = tile'}
                MV.write tiles o $ optile{CPU.tile = otile'}
                return (tiles, ins, outs)
              else return (tiles, ins, outs)
      Tiles.WaitingOnWrite p -> do
        if r == rows - 1
          then return (tiles, ins, outs)
          else do
            let o = getOtherTile i p
            optile <- MV.read tiles o
            let otile = CPU.tile optile
            let op = Tiles.getOppositePort p
            if writable otile op
              then do
                let (otile', val) = readValueFrom otile op
                let tile' = writeValueTo tile p $ fromJust val
                MV.write tiles i $ ptile{CPU.tile = tile'}
                MV.write tiles o $ optile{CPU.tile = otile'}
                return (tiles, ins, outs)
              else return (tiles, ins, outs)
      _ -> return (tiles, ins, outs)

  {-
    case tile of
      CPU.T21' t -> processT21Comm i (tiles, ins, outs)

  processT21Comm :: Int -> (RWTileVector, IODef, IODef) -> IO (RWTileVector, IODef, IODef)
  processT21Comm i (tiles, ins, outs) = do
    ptile <- MV.read tiles i
    let tile = CPU.tile ptile
    let (r, c) = CPU.pos ptile
    case T21.getTileRunState tile of
      T21.WaitingOnRead p -> do
        if r == 0
          then return (tiles, ins, outs)
          else return $ do
            let o = getOtherTile i p
            otile <- MV.read tiles o
            let op = T21.getOppositePort p
            if T21.getTileRunState otile == T21.WaitingOnWrite op
              then do
                let val = fromMaybe $ T21.getPortVal p otile
                let otile = T21.setTileRunState otile T21.Ready
                let otile = T21.clearPortVal op otile
                let tile = T21.setTileRunState tile T21.Ready
                let tile = T21.setPortVal p val tile
                MV.write tiles i tile
                MV.write tiles o otile
                return (tiles, ins, outs)
              else return (tiles, ins, outs)
      -}

  getOtherTile :: Int -> Tiles.Port' -> Int
  getOtherTile i p = case p of
    Tiles.LEFT -> i - 1
    Tiles.RIGHT -> i + 1
    Tiles.UP -> i - cols
    Tiles.DOWN -> i + cols
    Tiles.ANY -> i
    Tiles.LAST -> i

processTiles :: SimState -> IO SimState
processTiles = _
