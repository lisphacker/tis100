module TIS100.Sim.ConnectedTile where

class IsConnectedTile t

data ConnectedTile
  = forall t.
    (IsConnectedTile t) =>
    ConnectedTile t
