{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Parser where

import Control.Applicative ((<|>))
import Control.Applicative qualified as Applicative
import Control.Monad qualified as Monad
import Data.Aeson (ToJSON)
import Data.Text.Lazy (LazyText)
import Data.Text.Lazy qualified as Text.Lazy
import Data.Text.Lazy.IO qualified as Text.Lazy.IO
import Data.Void (Void)
import GHC.Generics (Generic)
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.Char qualified as Megaparsec.Char

type Parser a = Parsec Void LazyText a

data Header = Header
  { author :: LazyText,
    subject :: LazyText,
    messageID :: LazyText,
    inReplyTo :: Maybe LazyText,
    references :: Maybe LazyText,
    date :: LazyText
  }
  deriving (Generic, Show, ToJSON)

data Message
  = Message
  { content :: LazyText,
    author :: LazyText,
    subject :: LazyText,
    messageID :: LazyText,
    inReplyTo :: Maybe LazyText,
    references :: Maybe LazyText,
    date :: LazyText
  }
  deriving (Generic, Show, ToJSON)

preambleP :: Parser ()
preambleP = do
  Monad.void (Megaparsec.Char.string "From")
  Monad.void (Megaparsec.skipManyTill (Megaparsec.anySingleBut '\n') Megaparsec.Char.newline)

authorP :: Parser LazyText
authorP = do
  Monad.void (Megaparsec.Char.string "From: ")
  Monad.void (Megaparsec.skipMany (Megaparsec.anySingleBut '('))
  name <-
    Text.Lazy.pack
      <$> Megaparsec.between
        (Megaparsec.Char.char '(')
        (Megaparsec.Char.char ')')
        (Applicative.many (Megaparsec.anySingleBut ')'))
  Monad.void (Megaparsec.Char.newline)
  pure name

lineRemainderP :: Parser LazyText
lineRemainderP = do
  prefix <- singleLineRemainder
  rest <- Applicative.many (Megaparsec.Char.hspace1 *> singleLineRemainder)
  pure (Text.Lazy.intercalate " " (prefix : rest))
  where
    singleLineRemainder =
      Text.Lazy.pack <$> Megaparsec.someTill Megaparsec.anySingle Megaparsec.Char.newline

dateP :: Parser LazyText
dateP = do
  Monad.void (Megaparsec.Char.string "Date: ")
  lineRemainderP

subjectP :: Parser LazyText
subjectP = do
  Monad.void (Megaparsec.Char.string "Subject: ")
  lineRemainderP

inReplyToP :: Parser LazyText
inReplyToP = do
  Monad.void (Megaparsec.Char.string "In-Reply-To: ")
  lineRemainderP

referencesP :: Parser LazyText
referencesP = do
  Monad.void (Megaparsec.Char.string "References: ")
  lineRemainderP

messageIDP :: Parser LazyText
messageIDP = do
  Monad.void (Megaparsec.Char.string "Message-ID: ")
  lineRemainderP

contentP :: Parser LazyText
contentP = do
  Text.Lazy.pack
    <$> Megaparsec.someTill
      Megaparsec.anySingle
      (Megaparsec.lookAhead (Monad.void (Megaparsec.try headerP)) <|> Megaparsec.eof)

headerP :: Parser Header
headerP = do
  preambleP
  author <- authorP
  date <- dateP
  subject <- subjectP
  inReplyTo <- Applicative.optional inReplyToP
  references <- Applicative.optional referencesP
  messageID <- messageIDP
  pure Header {..}

messageP :: Parser Message
messageP = do
  Header {..} <- headerP
  content <- contentP
  pure Message {..}

testParser :: IO ()
testParser = do
  text <- Text.Lazy.IO.readFile "./data/may.txt"
  case Megaparsec.runParser (Applicative.many messageP) "input" text of
    Left errBundle -> putStrLn (Megaparsec.errorBundlePretty errBundle)
    Right result -> print result
