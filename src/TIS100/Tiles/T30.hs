module TIS100.Tiles.T30 where

import TIS100.Tiles.Base (Value (..))
import TIS100.Tiles.ConnectedTile (ConnectedTile (..))

newtype T30 = T30 [Value]
  deriving (Eq, Show)

instance ConnectedTile T30 where
  readValueFrom (T30 []) _ = (T30 [], Nothing)
  readValueFrom (T30 (v : vs)) _ = (T30 vs, Just v)
  writeValueTo (T30 vs) _ v = T30 (vs ++ [v])
  step = id