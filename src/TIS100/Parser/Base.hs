module TIS100.Parser.Base where

import Control.Monad (void)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (eof), Parsec, manyTill, oneOf, some, (<|>))
import Text.Megaparsec.Char (printChar, spaceChar)

type Parser = Parsec Void String

parseInt :: Parser Int
parseInt = do
  n <- some $ oneOf ['0' .. '9']
  return $ read n

parseToken :: Parser String
parseToken = do
  manyTill printChar (void spaceChar <|> eof)
