module TIS100.Sim.ConnectedTile where

import TIS100.Tiles.Inactive
import TIS100.Tiles.T21
import TIS100.Tiles.T30

class (Show t) => IsConnectedTile t

data ConnectedTile
  = forall t.
    (IsConnectedTile t) =>
    ConnectedTile t

instance IsConnectedTile T21

instance IsConnectedTile T30

instance IsConnectedTile InactiveTile

instance Show ConnectedTile where
  show (ConnectedTile t) = show t