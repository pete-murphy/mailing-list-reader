{-# LANGUAGE BlockArguments #-}
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
import Control.Arrow ((<<<), (>>>))
import Control.Monad qualified as Monad
import Data.Aeson (FromJSON, ToJSON)
import Data.Function ((&))
import Data.Generics.Labels ()
import Data.Maybe qualified as Maybe
import Data.Text.Lazy (LazyText)
import Data.Text.Lazy qualified as Text.Lazy
import Data.Time (ZonedTime)
import Data.Time qualified as Time
import Data.Void (Void)
import Debug.Trace qualified
import GHC.Generics (Generic)
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.Char qualified as Megaparsec.Char
import Text.Parsec.Rfc2822 qualified as Parsec.Rfc2822

-- Define the format string according to the input format
timestampFormat :: String
timestampFormat = "%e %b %Y %H:%M:%S %z"

-- Function to parse a timestamp string into a ZonedTime
parseTimestamp :: String -> Maybe ZonedTime
parseTimestamp str = do
  Debug.Trace.traceM str
  drop 4 str
    & Time.parseTimeM True Time.defaultTimeLocale timestampFormat

type Parser a = Parsec Void LazyText a

data Header = Header
  { author :: LazyText,
    subject :: LazyText,
    messageID :: LazyText,
    inReplyTo :: Maybe LazyText,
    references :: Maybe LazyText,
    date :: ZonedTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data Message = Message
  { content :: LazyText,
    author :: LazyText,
    subject :: LazyText,
    messageID :: LazyText,
    inReplyTo :: Maybe LazyText,
    references :: Maybe LazyText,
    date :: ZonedTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)

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

dateP :: Parser ZonedTime
dateP = do
  Monad.void (Megaparsec.Char.string "Date: ")
  remainder <- lineRemainderP
  case parseTimestamp (Text.Lazy.unpack remainder) of
    Just date -> pure date
    Nothing -> fail ("Could not parse date\n" <> show remainder)

subjectP :: Parser LazyText
subjectP = do
  Monad.void (Megaparsec.Char.string "Subject: ")
  lineRemainderP

inReplyToP :: Parser LazyText
inReplyToP = do
  Monad.void (Megaparsec.Char.string "In-Reply-To: ")
  lineRemainder <- lineRemainderP
  -- Sometimes this looks like
  --
  --   In-Reply-To: <ID> (So-and-so's message of "Some date")
  --
  -- and we want to extract only the ID.
  pure (Text.Lazy.takeWhile (/= ' ') lineRemainder)

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
  result <-
    Text.Lazy.pack
      <$> Megaparsec.someTill
        Megaparsec.anySingle
        (Megaparsec.lookAhead (Monad.void nextPartP <|> Monad.void (Megaparsec.try headerP)) <|> Megaparsec.eof)
  Monad.void (Applicative.optional nextPartP)
  pure result

nextPartP :: Parser String
nextPartP = do
  Monad.void ("-------------- next part --------------")
  Megaparsec.manyTill
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
  content' <- contentP
  let content =
        content'
          & Text.Lazy.strip
          & Text.Lazy.lines
          & reverse
          & span (\ln -> Text.Lazy.isPrefixOf "> " ln || Text.Lazy.null ln)
          & \case
            ([], rest) -> rest
            (_, rest) -> dropWhile (\ln -> (Text.Lazy.isPrefixOf "On " ln && Text.Lazy.isSuffixOf "> wrote:" ln)) rest
          & reverse
          & Text.Lazy.unlines
  pure Message {..}
