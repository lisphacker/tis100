module TIS100.Parser.Base where

import Control.Monad (void)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (eof), Parsec, manyTill, oneOf, optional, some, (<|>))
import Text.Megaparsec.Char (char, printChar, spaceChar)

type Parser = Parsec Void String

parseInt :: Parser Int
parseInt = do
  neg <- optional $ char '-'
  n <- some $ oneOf ['0' .. '9']
  return $ case neg of
    Just _ -> negate $ read n
    Nothing -> read n

parseToken :: Parser String
parseToken = do
  manyTill printChar (void spaceChar <|> eof)
