module TIS100.Sim.CPU where

import Data.Vector.Mutable
import TIS100.Parser.AsmParser (AsmSource)
import TIS100.Parser.Config
import TIS100.Sim.ConnectedTile (ConnectedTile (..))
import TIS100.Tiles.T21 (T21 (..))
import TIS100.Tiles.T30 (T30 (..))

data Tile = Tile
  { pos :: (Int, Int)
  , node :: ConnectedTile
  }

-- deriving (Eq, Show)

data CPUState m = CPUState
  { rows :: Int
  , cols :: Int
  , tiles :: MVector m Tile
  }

-- deriving (Eq, Show)

{-
createInitialState :: (Monad m) => Config -> AsmSource -> m (CPUState m)
createInitialState cfg asm = do
  let rows = rows cfg
  let cols = cols cfg
  let nodesTypes = nodes cfg

  mapM createNodeRow nodesTypes
 where
  createNodeRow :: (Monad m) => [NodeType] -> m (NodeRow m)
  createNodeRow nodeTypes' = do
    mapM createNode nodeTypes'

  createNode :: (Monad m) => NodeType -> m Node
  createNode nodeType = do
    case nodeType of
      Conpute -> return $ Node21 $ T21 asm
      Stack -> return $ Node30 $ T30
      Disabled -> return InactiveNode
-}