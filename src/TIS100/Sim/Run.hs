module TIS100.Sim.Run where

import Control.Monad (foldM)
import Control.Monad.ST
import Data.Vector qualified as MV
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import Foreign qualified as V
import TIS100.Parser.Config (IODef)
import TIS100.Parser.Config qualified as CFG
import TIS100.Sim.CPU qualified as CPU
import TIS100.Tiles.Inactive qualified as Inactive
import TIS100.Tiles.T21 qualified as T21
import TIS100.Tiles.T30 qualified as T30

data SimState = SimState
  { cpu :: CPU.CPUState
  , inputs :: CFG.IODef
  , outputs :: CFG.IODef
  }
  deriving (Show)

{-
class (Show t) => IsConnectedTile t where
  processTileComm :: t -> (Int, Int) -> (Int, Int) -> (MV.MVector RealWorld CPU.Tile, IODef, IODef) -> IO (MV.MVector RealWorld CPU.Tile, IODef, IODef)

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
processComm (SimState (CPU.CPUState rows cols tiles) ins outs) = do
  mtiles <- V.thaw tiles
  let nTiles = rows * cols
  (mtiles', ins', outs') <- foldM processTileComm' (mtiles, ins, outs) [0 .. nTiles - 1]
  tiles' <- V.freeze mtiles'
  return $ SimState (CPU.CPUState r c tiles') ins' outs'
 where
  processComm' :: (MV.MVector RealWorld CPU.Tile, IODef, IODef) -> Int -> IO (MV.MVector RealWorld CPU.Tile, IODef, IODef)
  processComm' (tiles, ins, outs) i = do
    tile <- MV.read tiles i
    case tile of
      CPU.T21' t -> processT21Comm i (tiles, ins, outs)

  processT21Comm :: Int -> (MV.MVector RealWorld CPU.Tile, IODef, IODef) -> IO (MV.MVector RealWorld CPU.Tile, IODef, IODef)
  processT21Comm i (tiles, ins, outs) = do
    tile <- MV.read tiles i
    let (r, c) = CPU.pos tile
    case T21.getTileRunState tile of
      T21.WaitingOnRead p -> do
        let o = getOtherTile i p
        otile <- MV.read tiles o
        let op = T21.getOppositePort p
        if T21.getTileRunState otile == T21.WaitingOnWrite op
          then do
            let val = fromMaybe $ getPortVal p otile
            let otile = T21.setTileRunState otile T21.Ready
            let otile = T21.clearPortVal op otile
            let tile = T32.setTileRunState tile T21.Ready
            let tile = T21.setPortVal p val tile
            MV.write tiles i tile
            MV.write tiles o otile
            return (tiles, ins, outs)
           
processTiles :: SimState -> IO SimState
processTiles = _
