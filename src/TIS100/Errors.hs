module TIS100.Errors where

data TISErrorCode = TISParseError
  deriving (Eq, Show)

data TISError = TISError TISErrorCode String
  deriving (Eq, Show)

type TISErrorOr a = Either TISError a