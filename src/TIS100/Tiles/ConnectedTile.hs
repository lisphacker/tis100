module TIS100.Tiles.ConnectedTile where

import TIS100.Tiles.Base (Port', Value)
import TIS100.Tiles.Base qualified as Tiles

class (Show t) => IsConnectedTile t where
  getRunState :: t -> Tiles.RunState
  setRunState :: t -> Tiles.RunState -> t

  readable :: t -> Port' -> Bool
  writable :: t -> Port' -> Bool

  isWaitingOnRead :: t -> Maybe Port'
  isWaitingOnWrite :: t -> Maybe Port'

  readValueFrom :: t -> Port' -> (t, Maybe Value)
  writeValueTo :: t -> Port' -> Value -> t

  step :: t -> t

data ConnectedTile
  = forall t.
    (IsConnectedTile t) =>
    ConnectedTile t

instance Show ConnectedTile where
  show (ConnectedTile t) = show t

instance IsConnectedTile ConnectedTile where
  getRunState (ConnectedTile t) = getRunState t
  setRunState (ConnectedTile t) rs = ConnectedTile $ setRunState t rs

  readable (ConnectedTile t) = readable t
  writable (ConnectedTile t) = writable t

  isWaitingOnRead (ConnectedTile t) = isWaitingOnRead t
  isWaitingOnWrite (ConnectedTile t) = isWaitingOnWrite t

  readValueFrom (ConnectedTile t) p = (ConnectedTile t', v) where (t', v) = readValueFrom t p
  writeValueTo (ConnectedTile t) p v = ConnectedTile $ writeValueTo t p v

  step (ConnectedTile t) = ConnectedTile $ step t
