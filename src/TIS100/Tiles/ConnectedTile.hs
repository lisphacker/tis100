module TIS100.Tiles.ConnectedTile where

import TIS100.Tiles.Base (Port', Value)

class ConnectedTile t where
  readValueFrom :: t -> Port' -> (t, Maybe Value)
  writeValueTo :: t -> Port' -> Value -> t
  step :: t -> t