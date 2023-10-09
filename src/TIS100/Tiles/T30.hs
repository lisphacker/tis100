module TIS100.Tiles.T30 where

import TIS100.Tiles.Base qualified as Tiles
import TIS100.Tiles.ConnectedTile (ConnectedTile (..), IsConnectedTile (..))

newtype T30 = T30 [Tiles.Value]
  deriving (Eq, Show)

instance IsConnectedTile T30 where
  getRunState _ = Tiles.Ready
  setRunState t _ = t

  readable (T30 []) _ = False
  readable _ _ = True
  writable _ _ = True

  isWaitingOnRead _ = Just Tiles.ANY
  isWaitingOnWrite _ = Just Tiles.ANY

  readValueFrom (T30 []) _ = (T30 [], Nothing)
  readValueFrom (T30 (v : vs)) _ = (T30 vs, Just v)
  writeValueTo (T30 vs) _ v = T30 (vs ++ [v])

  step = id
