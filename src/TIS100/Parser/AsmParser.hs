module TIS100.Parser.AsmParser where

import Control.Monad (void)
import Data.IntMap qualified as IM
import TIS100.Errors (TISError (..), TISErrorCode (..), TISErrorOr)
import TIS100.Parser.Base (Parser, parseInt)
import Text.Megaparsec (MonadParsec (eof, try), parse, sepBy, sepEndBy, some, (<|>))
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, space, string)

data Register = ACC | NIL | LEFT | RIGHT | UP | DOWN | ANY | LAST
  deriving (Eq, Show)

data RegisterOrConstant = Register Register | Constant Int
  deriving (Eq, Show)

data LabelOrInstruction
  = Label String
  | NOP
  | MOV RegisterOrConstant Register
  | SWP
  | SAV
  | ADD RegisterOrConstant
  | SUB RegisterOrConstant
  | NEG
  | JMP String
  | JEZ String
  | JNZ String
  | JGZ String
  | JLZ String
  | JRO RegisterOrConstant
  deriving (Eq, Show)

type TileAsmSource = [LabelOrInstruction]

type AsmSource = IM.IntMap TileAsmSource

parseLabel :: Parser String
parseLabel = some alphaNumChar

parseLabelDef :: Parser LabelOrInstruction
parseLabelDef = do
  label <- parseLabel
  _ <- char ':'
  return $ Label label

parseRegister :: Parser Register
parseRegister = do
  reg <- some letterChar
  case reg of
    "ACC" -> return ACC
    "NIL" -> return NIL
    "LEFT" -> return LEFT
    "RIGHT" -> return RIGHT
    "UP" -> return UP
    "DOWN" -> return DOWN
    "ANY" -> return ANY
    "LAST" -> return LAST
    _ -> fail $ "Invalid register: " ++ reg

parseRegisterOrConstant :: Parser RegisterOrConstant
parseRegisterOrConstant = try (Register <$> parseRegister) <|> Constant <$> parseInt

parseNOP :: Parser LabelOrInstruction
parseNOP = do
  _ <- string "NOP"
  return NOP

parseMOV :: Parser LabelOrInstruction
parseMOV = do
  _ <- string "MOV"
  space
  src <- parseRegisterOrConstant
  space
  _ <- char ','
  space
  MOV src <$> parseRegister

parseSWP :: Parser LabelOrInstruction
parseSWP = do
  _ <- string "SWP"
  return SWP

parseSAV :: Parser LabelOrInstruction
parseSAV = do
  _ <- string "SAV"
  return SAV

parseADD :: Parser LabelOrInstruction
parseADD = do
  _ <- string "ADD"
  space
  ADD <$> parseRegisterOrConstant

parseSUB :: Parser LabelOrInstruction
parseSUB = do
  _ <- string "SUB"
  space
  SUB <$> parseRegisterOrConstant

parseNEG :: Parser LabelOrInstruction
parseNEG = do
  _ <- string "NEG"
  return NEG

parseJump :: String -> (String -> LabelOrInstruction) -> Parser LabelOrInstruction
parseJump ins constructor = do
  _ <- string ins
  space
  constructor <$> parseLabel

parseJMP :: Parser LabelOrInstruction
parseJMP = parseJump "JMP" JMP

parseJEZ :: Parser LabelOrInstruction
parseJEZ = parseJump "JEZ" JEZ

parseJNZ :: Parser LabelOrInstruction
parseJNZ = parseJump "JNZ" JNZ

parseJGZ :: Parser LabelOrInstruction
parseJGZ = parseJump "JGZ" JGZ

parseJLZ :: Parser LabelOrInstruction
parseJLZ = parseJump "JLZ" JLZ

parseJRO :: Parser LabelOrInstruction
parseJRO = do
  _ <- string "JRO"
  space
  JRO <$> parseRegisterOrConstant

parseInstruction :: Parser LabelOrInstruction
parseInstruction = try parseNOP <|> try parseMOV <|> try parseSWP <|> try parseSAV <|> try parseADD <|> try parseSUB <|> try parseNEG <|> try parseJMP <|> try parseJEZ <|> try parseJNZ <|> try parseJGZ <|> try parseJLZ <|> try parseJRO

parseLabelOrInstruction :: Parser LabelOrInstruction
parseLabelOrInstruction = try parseLabelDef <|> try parseInstruction

parseTileAsm :: Parser (Int, TileAsmSource)
parseTileAsm = do
  _ <- char '@'
  n <- parseInt
  space
  labelsOrInstructions <- sepEndBy (try parseLabelOrInstruction') $ try endOfTileProgram
  return (n, labelsOrInstructions)
 where
  parseLabelOrInstruction' = do
    li <- parseLabelOrInstruction
    void space <|> eof
    return li
  endOfTileProgram = void space <|> void (char '@') <|> eof

parseAllAsm :: Parser AsmSource
parseAllAsm = do
  sources <- sepBy parseTileAsm space
  return $ IM.fromList sources

parseAsm :: String -> TISErrorOr AsmSource
parseAsm asmSrc = case parse parseAllAsm "tis100src" asmSrc of
  Left err -> Left $ TISError TISParseError $ show err
  Right cfg -> Right cfg
