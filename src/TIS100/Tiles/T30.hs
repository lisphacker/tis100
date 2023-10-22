module TIS100.Tiles.T30 where

import TIS100.Tiles.Base qualified as Tiles
import TIS100.Tiles.ConnectedTile (IsConnectedTile (..))

newtype T30 = T30 [Tiles.Value]
  deriving (Eq, Show)

instance IsConnectedTile T30 where
  getRunState _ = Tiles.Ready
  setRunState _ t = t

  readable _ (T30 []) = False
  readable _ _ = True
  writable _ _ = True

  isWaitingOnRead _ = Just Tiles.ANY
  isWaitingOnWrite _ = Just Tiles.ANY

  readValueFrom _ (T30 []) = (T30 [], Nothing)
  readValueFrom _ (T30 (v : vs)) = (T30 vs, Just v)
  writeValueTo _ v (T30 vs) = Just $ T30 (vs ++ [v])

  step = id
