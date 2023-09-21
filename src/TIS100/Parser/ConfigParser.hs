module TIS100.Parser.ConfigParser where

import Control.Monad (replicateM)
import Data.Void (Void)
import TIS100.Errors (TISError (..), TISErrorCode (TISParseError), TISErrorOr)
import Text.Megaparsec (Parsec, count, oneOf, parse, some)
import Text.Megaparsec.Char (space)

type Parser = Parsec Void String

data NodeType = Conpute | Stack | Disabled
  deriving (Eq, Show)

data IOType = None | StdIO | List [Int] | File FilePath
  deriving (Eq, Show)

data Config = Config
  { rows :: Int,
    cols :: Int,
    nodes :: [[NodeType]],
    inputs :: [IOType],
    outputs :: [IOType]
  }
  deriving (Eq, Show)

parseRow :: Int -> Parser [NodeType]
parseRow n = do
  space
  nodes <- count n $ oneOf ['C', 'S', 'D']
  return $ map parseNodeType nodes
  where
    parseNodeType 'C' = Conpute
    parseNodeType 'S' = Stack
    parseNodeType 'D' = Disabled

intParser :: Parser Int
intParser = do
  space
  n <- some $ oneOf ['0' .. '9']
  return $ read n

cfgParser :: Parser Config
cfgParser = do
  rows <- intParser
  cols <- intParser
  nodes <- replicateM rows (parseRow cols)
  return $ Config rows cols nodes [] []

parseConfig :: String -> TISErrorOr Config
parseConfig cfgSrc = case parse cfgParser "tis100src" cfgSrc of
  Left err -> Left $ TISError TISParseError $ show err
  Right cfg -> Right cfg