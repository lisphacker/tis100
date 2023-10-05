module TIS100.Sim.CPU where

import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import TIS100.Parser.AsmParser (AsmSource)
import TIS100.Parser.Config qualified as C
import TIS100.Sim.ConnectedTile (ConnectedTile (..))
import TIS100.Tiles.T21 qualified as T21
import TIS100.Tiles.T30 qualified as T30

-- data TileType = Undefined | T21 | T30
--   deriving (Eq, Show)

data Tile = Tile
  { pos :: (Int, Int)
  , -- , typ :: TileType
    connTile :: ConnectedTile
  }

data CPUState = CPUState
  { rows :: Int
  , cols :: Int
  , tiles :: V.Vector Tile
  }

createInitialCPUState :: C.Config -> AsmSource -> CPUState
createInitialCPUState cfg asm =
  let rows = C.rows cfg
      cols = C.cols cfg
      numTiles = rows * cols
      tileTypes = concat $ C.tiles cfg
   in CPUState rows cols $ V.fromList $ zipWith createTile [0 ..] tileTypes
 where
  createTile :: Int -> C.TileType -> Tile
  createTile i tileType =
    let pos = i `divMod` C.cols cfg
     in case tileType of
          C.Conpute -> Tile pos $ ConnectedTile $ T21.createTileState []
          C.Stack -> Tile pos $ ConnectedTile $ T30.T30 []
          C.Disabled -> Tile pos $ ConnectedTile $ T21.createTileState []

{-

createInitialState :: (Monad m) => C.Config -> AsmSource -> m (CPUState (m ()))
createInitialState cfg asm = do
  let rows = C.rows cfg
  let cols = C.cols cfg
  let numTiles = rows * cols
  let tileTypes = concat $ C.tiles cfg

  tileVector <- MV.generate numTiles $ createTile tileTypes
  return $ CPUState rows cols tileVector
 where
  createTile :: [C.TileType] -> Int -> Tile
  createTile tileTypes i =
    let tileType = tileTypes !! i
     in case tileType of
          C.Conpute -> Tile (0, 0) $ ConnectedTile $ T21
          C.Stack -> Tile (0, 0) $ ConnectedTile $ T30
          C.Disabled -> Tile (0, 0) $ ConnectedTile $ T21
-}