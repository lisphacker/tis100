module TIS100.Tiles.Inactive where

import TIS100.Tiles.ConnectedTile (ConnectedTile (..))

data InactiveTile = InactiveTile
  deriving (Eq, Show)

instance ConnectedTile InactiveTile where
  readValueFrom t _ = (t, Nothing)
  writeValueTo t _ _ = t
  step = id
