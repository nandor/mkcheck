-- This file is part of the mkcheck project.
-- Licensing information can be found in the LICENSE file.
-- (C) 2017 Nandor Licker. All rights reserved.

module Parser (parseMakefile) where

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.Text (Parser)
import           Data.Attoparsec.Text as Atto
import           Data.Char
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Makefile

import Debug.Trace



parseMakefile :: Text -> Either String Makefile
parseMakefile text
  = parseOnly makefile text


makefile :: Parser Makefile
makefile = do
  skipComments
  entries <- Atto.many' ((assignment <|> rule) <* skipComments)
  Atto.endOfInput
  return $ Makefile entries


skipComments :: Parser ()
skipComments = do
  Atto.skipWhile (== '\n')
  Atto.peekChar >>= \case
    Just '#' -> do
      Atto.skipWhile (/= '\n')
      skipComments
    _ ->
      return ()

takeLineTill :: (Char -> Bool) -> Parser Text
takeLineTill cond
  = Atto.takeTill (\ch -> ch == '\n' || cond ch)


assignment :: Parser Entry
assignment = Atto.try $ do
  name <- Text.unpack . Text.strip <$> takeLineTill (== '=')
  Atto.char '='
  value <- Text.unpack . Text.strip <$> Atto.takeTill (== '\n')
  return $ Assignment name value


rule :: Parser Entry
rule = Atto.try $ do
  output <- Atto.takeWhile (\ch -> not (isSpace ch) && ch /= ':')
  Atto.skipWhile isSpace
  char ':'
  input <- Text.strip <$> Atto.takeTill (== '\n')
  char '\n'

  commands <- many $ do
    char '\t'
    line <- Atto.takeTill (== '\n')
    char '\n'
    return line

  return $ Rule
    { mrOutput = Text.unpack output
    , mrInputs = filter (/= []) . map Text.unpack . Text.split isSpace $ input
    , mrCommands = map Text.unpack commands
    }
