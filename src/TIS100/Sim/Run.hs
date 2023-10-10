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

runStep :: SimState -> IO SimState
runStep = processComm >> stepTiles

processComm :: SimState -> IO SimState
processComm (SimState (CPU.CPUState (CPU.CPUConfig rows cols) tiles) ins outs) = do
  mtiles <- V.thaw tiles
  let nTiles = rows * cols
  (mtiles', ins', outs') <- foldM processTileComm (mtiles, ins, outs) [0 .. nTiles - 1]
  tiles' <- V.freeze mtiles'
  return $ SimState (CPU.CPUState (CPU.CPUConfig rows cols) tiles') ins' outs'
 where
  processTileComm :: (RWTileVector, IODef, IODef) -> Int -> IO (RWTileVector, IODef, IODef)
  processTileComm (tiles, ins, outs) i = do
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
            if readable op otile
              then do
                let (tile', val) = readValueFrom p tile
                let otile' = writeValueTo op (fromJust val) otile
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
            if writable op otile
              then do
                let (otile', val) = readValueFrom op otile
                let tile' = writeValueTo p (fromJust val) tile
                MV.write tiles i $ ptile{CPU.tile = tile'}
                MV.write tiles o $ optile{CPU.tile = otile'}
                return (tiles, ins, outs)
              else return (tiles, ins, outs)
      _ -> return (tiles, ins, outs)

  getOtherTile :: Int -> Tiles.Port' -> Int
  getOtherTile i p = case p of
    Tiles.LEFT -> i - 1
    Tiles.RIGHT -> i + 1
    Tiles.UP -> i - cols
    Tiles.DOWN -> i + cols
    Tiles.ANY -> i
    Tiles.LAST -> i

stepTiles :: SimState -> IO SimState
stepTiles (SimState (CPU.CPUState (CPU.CPUConfig rows cols) tiles) ins outs) = do
  mtiles <- V.thaw tiles
  let nTiles = rows * cols
  (mtiles', ins', outs') <- foldM stepTile (mtiles, ins, outs) [0 .. nTiles - 1]
  tiles' <- V.freeze mtiles'
  return $ SimState (CPU.CPUState (CPU.CPUConfig rows cols) tiles') ins' outs'
 where
  stepTile :: (RWTileVector, IODef, IODef) -> Int -> IO (RWTileVector, IODef, IODef)
  stepTile (tiles, ins, outs) i = do
    ptile <- MV.read tiles i
    let tile = CPU.tile ptile
    let tile' = step tile
    let ptile' = ptile{CPU.tile = tile'}
    MV.write tiles i ptile'
    return $ (tiles, ins, outs)