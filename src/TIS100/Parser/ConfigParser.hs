module TIS100.Parser.ConfigParser where

import Control.Monad (replicateM)
import Data.IntMap qualified as IM
import Data.Void (Void)
import Debug.Trace (traceM)
import TIS100.Errors (TISError (..), TISErrorCode (TISParseError), TISErrorOr)
import Text.Megaparsec (MonadParsec (eof, takeWhile1P, try), Parsec, anySingleBut, count, manyTill, oneOf, parse, some, (<|>))
import Text.Megaparsec.Char (char, printChar, space, spaceChar, string)

type Parser = Parsec Void String

data NodeType = Conpute | Stack | Disabled
  deriving (Eq, Show)

data IOSource = StdIO | List [Int] | File FilePath
  deriving (Eq, Show)

type IODef = IM.IntMap IOSource

data Config = Config
  { rows :: Int,
    cols :: Int,
    nodes :: [[NodeType]],
    inputs :: IODef,
    outputs :: IODef
  }
  deriving (Eq, Show)

data Direction = Input | Output
  deriving (Eq, Show)

intParser :: Parser Int
intParser = do
  n <- some $ oneOf ['0' .. '9']
  return $ read n

intListParser :: Parser [Int]
intListParser = do
  some $ do
    n <- intParser
    space
    return n

parseRow :: Int -> Parser [NodeType]
parseRow n = do
  nodes <- count n $ oneOf ['C', 'S', 'D']
  return $ map parseNodeType nodes
  where
    parseNodeType 'C' = Conpute
    parseNodeType 'S' = Stack
    parseNodeType 'D' = Disabled

parseToken :: Parser String
parseToken = do
  manyTill printChar spaceChar

parseIOSource :: Parser IOSource
parseIOSource = do
  space
  string "NUMERIC"
  space
  srcType <- parseToken
  space
  case srcType of
    "STDIN" -> return StdIO
    "STDOUT" -> return StdIO
    "-" -> List <$> intListParser
    "FILE" -> File <$> parseToken
    _ -> fail $ "Unknown IO source type: " ++ srcType

parseIODef :: Parser (Direction, Int, IOSource)
parseIODef = do
  space
  dir <- (\x -> if x == 'I' then Input else Output) <$> oneOf ['I', 'O']
  n <- intParser
  src <- parseIOSource
  return (dir, n, src)

parseIODefs :: IODef -> IODef -> Parser (IODef, IODef)
parseIODefs inputs outputs =
  do
    space
    try $ do
      eof
      return $ (inputs, outputs)
    <|> do
      (dir, n, iosrc) <- parseIODef
      parseIODefs (condInf n dir Input inputs iosrc) (condInf n dir Output outputs iosrc)
  where
    condInf :: Int -> Direction -> Direction -> IODef -> IOSource -> IODef
    condInf n dir refDir ioSrc ioDef = if dir == refDir then IM.insert n ioDef ioSrc else ioSrc

cfgParser :: Parser Config
cfgParser = do
  rows <- intParser
  space
  cols <- intParser
  space
  nodes <- replicateM rows $ do
    nodesRow <- parseRow cols
    space
    return nodesRow
  space
  (inputs, outputs) <- parseIODefs IM.empty IM.empty
  return $ Config rows cols nodes inputs outputs

parseConfig :: String -> TISErrorOr Config
parseConfig cfgSrc = case parse cfgParser "tis100src" cfgSrc of
  Left err -> Left $ TISError TISParseError $ show err
  Right cfg -> Right cfg