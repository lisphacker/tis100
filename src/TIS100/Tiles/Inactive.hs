module TIS100.Tiles.Inactive where

import TIS100.Tiles.Base qualified as Tiles
import TIS100.Tiles.ConnectedTile (IsConnectedTile (..))

data InactiveTile = InactiveTile
  deriving (Eq, Show)

instance IsConnectedTile InactiveTile where
  getRunState _ = Tiles.Ready
  setRunState _ _ = InactiveTile

  readable _ _ = False
  writable _ _ = False

  isWaitingOnRead _ = Just Tiles.ANY
  isWaitingOnWrite _ = Just Tiles.ANY

  readValueFrom _ t = (t, Nothing)
  writeValueTo _ _ t = Nothing

  step = id
