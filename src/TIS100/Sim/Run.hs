module TIS100.Sim.Run where

import Control.Monad
import Control.Monad.ST
import Data.IntMap qualified as IM
import Data.Maybe (fromJust)
import Data.Vector qualified as MV
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import TIS100.Parser.Config (IODef)
import TIS100.Parser.Config qualified as CFG
import TIS100.Sim.CPU qualified as CPU
import TIS100.Tiles.Base qualified as Tiles
import TIS100.Tiles.ConnectedTile (IsConnectedTile (..))

data SimState = SimState
  { cpu :: CPU.CPUState
  , inputs :: CFG.IODef
  , outputs :: CFG.IODef
  }
  deriving (Eq, Show)

type RWTileVector = MV.MVector RealWorld CPU.PositionedTile

dumpSimState :: String -> SimState -> IO ()
dumpSimState prefix s = do
  print $ prefix
  print $ "  " ++ show (((flip (V.!)) 1) . CPU.tiles . cpu $ s)
  print $ "  " ++ show (((flip (V.!)) 2) . CPU.tiles . cpu $ s)
  print $ "  IN1:  " ++ show (IM.lookup 1 $ inputs s)
  print $ "  IN2:  " ++ show (IM.lookup 2 $ inputs s)

loopUntilNoChange :: Int -> SimState -> IO SimState
loopUntilNoChange i s = do
  putStrLn ""
  putStrLn ""
  dumpSimState "Before: " s
  nextSimState <- runStep s
  dumpSimState "After: " nextSimState
  if nextSimState == s
    then return s
    else loopUntilNoChange (i + 1) nextSimState

run :: SimState -> IO SimState
run = loopUntilNoChange 1

runStep :: SimState -> IO SimState
-- runStep = processComm >=> stepTiles
runStep s = do
  s' <- processComm s
  dumpSimState "After comm: " s'
  stepTiles s'

readInputValue :: Int -> CFG.IODef -> IO (Maybe Int, CFG.IODef)
readInputValue ti iodef = case IM.lookup ti iodef of
  Just (CFG.List (v : vs)) -> return (Just v, IM.insert ti (CFG.List vs) iodef)
  Just (CFG.List []) -> return (Nothing, iodef)
  Just (CFG.File _) -> error "Tile I/O using files is not yet implemented"
  Just CFG.StdIO -> error "Tile I/O using StdIO is not yet implemented"
  Nothing -> return (Nothing, iodef)

writeOutputValue :: Int -> Int -> CFG.IODef -> IO CFG.IODef
writeOutputValue ti v iodef = case IM.lookup ti iodef of
  Just (CFG.List vs) -> return $ IM.insert ti (CFG.List (vs ++ [v])) iodef
  Just (CFG.File _) -> error "Tile I/O using files is not yet implemented"
  Just CFG.StdIO -> error "Tile I/O using StdIO is not yet implemented"
  Nothing -> return $ IM.insert ti (CFG.List [v]) iodef

processComm :: SimState -> IO SimState
processComm (SimState (CPU.CPUState (CPU.CPUConfig rows cols) tiles_) ins_ outs_) = do
  mtiles <- V.thaw tiles_
  let nTiles = rows * cols
  (mtiles', ins', outs') <- foldM processTileComm' (mtiles, ins_, outs_) [0 .. nTiles - 1]
  tiles' <- V.freeze mtiles'
  return $ SimState (CPU.CPUState (CPU.CPUConfig rows cols) tiles') ins' outs'
 where
  processTileComm' :: (RWTileVector, IODef, IODef) -> Int -> IO (RWTileVector, IODef, IODef)
  processTileComm' (tiles, ins, outs) i = do
    (tiles', ins', outs') <- processTileComm (tiles, ins, outs) i
    return (tiles', ins', outs')
  processTileComm :: (RWTileVector, IODef, IODef) -> Int -> IO (RWTileVector, IODef, IODef)
  processTileComm (tiles, ins, outs) i = do
    ptile <- MV.read tiles i
    let tile = CPU.tile ptile
    let (r, c) = CPU.position ptile

    case getRunState tile of
      Tiles.WaitingOnRead p -> do
        if r == 0 && p == Tiles.UP
          then do
            (maybeV, ins') <- readInputValue c ins
            case maybeV of
              Just v -> do
                let maybeTile' = writeValueTo p (Tiles.Value v) tile
                case maybeTile' of
                  Just tile' -> do
                    MV.write tiles i $ ptile{CPU.tile = tile'}
                    return (tiles, ins', outs)
                  Nothing -> return (tiles, ins, outs)
              Nothing -> return (tiles, ins, outs)
          else do
            let o = getOtherTile i p
            optile <- MV.read tiles o
            let otile = CPU.tile optile
            let op = Tiles.getOppositePort p
            if readable op otile
              then do
                let (otile', val) = readValueFrom op otile
                let maybeTile' = writeValueTo p (fromJust val) tile
                case maybeTile' of
                  Just tile' -> do
                    MV.write tiles i $ ptile{CPU.tile = tile'}
                    MV.write tiles o $ optile{CPU.tile = otile'}
                    return (tiles, ins, outs)
                  Nothing -> return (tiles, ins, outs)
              else return (tiles, ins, outs)
      Tiles.WaitingOnWrite p -> do
        if r == rows - 1 && p == Tiles.DOWN
          then do
            let (tile', maybeV) = readValueFrom p tile
            case maybeV of
              Just (Tiles.Value v) -> do
                outs' <- writeOutputValue c v outs
                MV.write tiles i $ ptile{CPU.tile = tile'}
                return (tiles, ins, outs')
              Nothing -> return (tiles, ins, outs)
          else do
            let o = getOtherTile i p
            optile <- MV.read tiles o
            let otile = CPU.tile optile
            let op = Tiles.getOppositePort p
            print $ "  Tile " ++ show o ++ " writable = " ++ show (writable op otile)
            if writable op otile
              then do
                let (tile', val) = readValueFrom p tile
                let maybeOtile' = writeValueTo op (fromJust val) otile
                case maybeOtile' of
                  Just otile' -> do
                    MV.write tiles i $ ptile{CPU.tile = tile'}
                    MV.write tiles o $ optile{CPU.tile = otile'}
                    return (tiles, ins, outs)
                  Nothing -> return (tiles, ins, outs)
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
  mtiles' <- foldM stepTile mtiles [0 .. nTiles - 1]
  tiles' <- V.freeze mtiles'
  return $ SimState (CPU.CPUState (CPU.CPUConfig rows cols) tiles') ins outs
 where
  stepTile :: RWTileVector -> Int -> IO RWTileVector
  stepTile tiles_ i = do
    ptile <- MV.read tiles_ i
    let tile = CPU.tile ptile
    let tile' = step tile
    let ptile' = ptile{CPU.tile = tile'}
    MV.write tiles_ i ptile'
    return tiles_