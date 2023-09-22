module TIS100.Parser.AsmParser where

import Control.Monad (void)
import Data.Graph (Tree (Node))
import Data.IntMap qualified as IM
import Debug.Trace (trace, traceM)
import TIS100.Errors (TISError (..), TISErrorCode (..), TISErrorOr)
import TIS100.Parser.Base (Parser, parseInt, parseToken)
import Text.Megaparsec (MonadParsec (eof, label, try), parse, sepBy, sepEndBy, some, (<|>))
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

newtype NodeAsmSource = NodeAsmSource [LabelOrInstruction]
  deriving (Eq, Show)

type AsmSource = IM.IntMap NodeAsmSource

parseLabel :: Parser String
parseLabel = some alphaNumChar

parseLabelDef :: Parser LabelOrInstruction
parseLabelDef = do
  label <- parseLabel
  char ':'
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
  string "NOP"
  return NOP

parseMOV :: Parser LabelOrInstruction
parseMOV = do
  string "MOV"
  space
  src <- parseRegisterOrConstant
  space
  char ','
  space
  MOV src <$> parseRegister

parseSWP :: Parser LabelOrInstruction
parseSWP = do
  string "SWP"
  return SWP

parseSAV :: Parser LabelOrInstruction
parseSAV = do
  string "SAV"
  return SAV

parseADD :: Parser LabelOrInstruction
parseADD = do
  string "ADD"
  space
  ADD <$> parseRegisterOrConstant

parseSUB :: Parser LabelOrInstruction
parseSUB = do
  string "SUB"
  space
  SUB <$> parseRegisterOrConstant

parseNEG :: Parser LabelOrInstruction
parseNEG = do
  string "NEG"
  return NEG

parseJump :: String -> (String -> LabelOrInstruction) -> Parser LabelOrInstruction
parseJump ins constructor = do
  string ins
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
  string "JRO"
  space
  JRO <$> parseRegisterOrConstant

parseInstruction :: Parser LabelOrInstruction
parseInstruction = try parseNOP <|> try parseMOV <|> try parseSWP <|> try parseSAV <|> try parseADD <|> try parseSUB <|> try parseNEG <|> try parseJMP <|> try parseJEZ <|> try parseJNZ <|> try parseJGZ <|> try parseJLZ <|> try parseJRO

parseLabelOrInstruction :: Parser LabelOrInstruction
parseLabelOrInstruction = try parseLabelDef <|> try parseInstruction

parseNodeAsm :: Parser (Int, NodeAsmSource)
parseNodeAsm = do
  char '@'
  n <- parseInt
  space
  labelsOrInstructions <- sepEndBy (try parseLabelOrInstruction') $ try endOfNodeProgram
  return (n, NodeAsmSource labelsOrInstructions)
  where
    parseLabelOrInstruction' = do
      li <- parseLabelOrInstruction
      void space <|> eof
      return li
    endOfNodeProgram = void space <|> void (char '@') <|> eof

parseAllAsm :: AsmSource -> Parser AsmSource
parseAllAsm nodeSources = do
  sources <- sepBy parseNodeAsm space
  return $ IM.fromList sources

parseAsm :: String -> TISErrorOr AsmSource
parseAsm asmSrc = case parse (parseAllAsm IM.empty) "tis100src" asmSrc of
  Left err -> Left $ TISError TISParseError $ show err
  Right cfg -> Right cfg
