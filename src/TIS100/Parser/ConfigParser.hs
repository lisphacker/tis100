module TIS100.Parser.ConfigParser where

import Control.Monad (replicateM)
import Data.IntMap qualified as IM
import System.FilePath (takeDirectory, (</>))
import TIS100.Errors (TISError (..), TISErrorCode (TISParseError), TISErrorOr)
import TIS100.Parser.Base (Parser, parseInt, parseToken)
import TIS100.Parser.Config (Config (Config, inputs, refOutputs), IODef, IOSource (..), TileType (..))
import Text.Megaparsec (MonadParsec (eof, try), count, oneOf, parse, some, (<|>))
import Text.Megaparsec.Char (space, string)

data Direction = Input | Output
  deriving (Eq, Show)

parseIntList :: Parser [Int]
parseIntList = do
  some $ do
    n <- parseInt
    space
    return n

parseRow :: Int -> Parser [TileType]
parseRow n = do
  nodes <- count n $ oneOf ['C', 'S', 'D']
  return $ map parseTileType nodes
 where
  parseTileType 'C' = Conpute
  parseTileType 'S' = Stack
  parseTileType 'D' = Disabled
  parseTileType tileType = error $ "Unknown tile type in config: " ++ [tileType]

parseIOSource :: Parser IOSource
parseIOSource = do
  space
  _ <- string "NUMERIC"
  space
  srcType <- parseToken
  space
  case srcType of
    "STDIN" -> return StdIO
    "STDOUT" -> return StdIO
    "-" -> List <$> parseIntList
    "FILE" -> File <$> parseToken
    _ -> fail $ "Unknown IO source type: " ++ srcType

parseIODef :: Parser (Direction, Int, IOSource)
parseIODef = do
  space
  dir <- (\x -> if x == 'I' then Input else Output) <$> oneOf ['I', 'O']
  n <- parseInt
  src <- parseIOSource
  return (dir, n, src)

parseIODefs :: IODef -> IODef -> Parser (IODef, IODef)
parseIODefs inputs_ outputs =
  do
    try $ do
      space
      eof
      return $ (inputs_, outputs)
    <|> do
      space
      (dir, n, iosrc) <- parseIODef
      parseIODefs (condInf n dir Input inputs_ iosrc) (condInf n dir Output outputs iosrc)
 where
  condInf :: Int -> Direction -> Direction -> IODef -> IOSource -> IODef
  condInf n dir refDir ioSrc ioDef = if dir == refDir then IM.insert n ioDef ioSrc else ioSrc

cfgParser :: Parser Config
cfgParser = do
  rows <- parseInt
  space
  cols <- parseInt
  space
  tiles <- replicateM rows $ do
    tilesRow <- parseRow cols
    space
    return tilesRow
  space
  (inputs_, refOutputs_) <- parseIODefs IM.empty IM.empty
  return $ Config rows cols tiles inputs_ IM.empty refOutputs_

parseConfig :: String -> TISErrorOr Config
parseConfig cfgSrc = case parse cfgParser "tis100cfg" cfgSrc of
  Left err -> Left $ TISError TISParseError $ show err
  Right cfg -> Right cfg

readExternalInputs :: FilePath -> Config -> IO Config
readExternalInputs cfgPath config = do
  inputs' <- mapM readExternalInput $ inputs config
  refOutputs' <- mapM readExternalInput $ refOutputs config
  return $ config{inputs = inputs', refOutputs = refOutputs'}
 where
  readExternalInput :: IOSource -> IO IOSource
  readExternalInput (File path) = do
    contents <- readFile $ takeDirectory cfgPath </> path
    return $ List $ map read $ words contents
  readExternalInput StdIO = do
    List . map read . words <$> getContents
  readExternalInput src = return src
