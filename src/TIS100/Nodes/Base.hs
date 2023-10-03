module TIS100.Nodes.Base where

newtype Value = Value Int
  deriving (Eq)

instance Show Value where
  show (Value v) = show v

clamp :: Int -> Int
clamp = max (-999) . min 999

instance Num Value where
  (+) (Value a) (Value b) = Value $ clamp (a + b)
  (*) (Value a) (Value b) = Value $ clamp (a * b)
  (-) (Value a) (Value b) = Value $ clamp (a - b)
  abs (Value a) = Value $ abs a
  signum (Value a) = Value $ signum a
  fromInteger = Value . clamp . fromInteger
