module TIS100.Tiles.ConnectedTile where

import TIS100.Tiles.Base (Port', Value)
import TIS100.Tiles.Base qualified as Tiles

class (Show t) => IsConnectedTile t where
  getRunState :: t -> Tiles.RunState
  setRunState :: Tiles.RunState -> t -> t

  readable :: Port' -> t -> Bool
  writable :: Port' -> t -> Bool

  isWaitingOnRead :: t -> Maybe Port'
  isWaitingOnWrite :: t -> Maybe Port'

  readValueFrom :: Port' -> t -> (t, Maybe Value)
  writeValueTo :: Port' -> Value -> t -> t

  step :: t -> t

data ConnectedTile
  = forall t.
    (IsConnectedTile t) =>
    ConnectedTile t

instance Show ConnectedTile where
  show (ConnectedTile t) = show t

instance IsConnectedTile ConnectedTile where
  getRunState (ConnectedTile t) = getRunState t
  setRunState rs (ConnectedTile t) = ConnectedTile $ setRunState rs t

  readable p (ConnectedTile t) = readable p t
  writable p (ConnectedTile t) = writable p t

  isWaitingOnRead (ConnectedTile t) = isWaitingOnRead t
  isWaitingOnWrite (ConnectedTile t) = isWaitingOnWrite t

  readValueFrom p (ConnectedTile t) = (ConnectedTile t', v) where (t', v) = readValueFrom p t
  writeValueTo p v (ConnectedTile t) = ConnectedTile $ writeValueTo p v t

  step (ConnectedTile t) = ConnectedTile $ step t
