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
import Control.Arrow ((<<<))
import Control.Monad qualified as Monad
import Control.Monad.Fail qualified as MonadFail
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.Function ((&))
import Data.Generics.Labels ()
import Data.MIME.Charset qualified
import Data.MIME.EncodedWord qualified
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as Text.Lazy
import Data.Time (ZonedTime)
import Data.Time qualified as Time
import Data.Time.Format.ISO8601 qualified as ISO8601
import Data.Void (Void)
import Database.SQLite.Simple (ToRow (..))
import Database.SQLite.Simple qualified as SQLite
import Database.SQLite.Simple.FromRow qualified as SQLite
import GHC.Generics (Generic)
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.Char qualified as Megaparsec.Char

timestampFormat :: String
timestampFormat = "%e %b %Y %H:%M:%S %z"

parseTimestamp :: String -> Maybe ZonedTime
parseTimestamp str = do
  -- Debug.Trace.traceM str
  drop 4 str
    & Time.parseTimeM True Time.defaultTimeLocale timestampFormat

type Parser a = Parsec Void Text a

data Header = Header
  { author :: Text,
    subject :: Text,
    messageID :: Text,
    inReplyTo :: Maybe Text,
    references :: Maybe Text,
    date :: ZonedTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data Message = Message
  { content :: Text,
    author :: Text,
    subject :: Text,
    messageID :: Text,
    inReplyTo :: Maybe Text,
    references :: Maybe Text,
    date :: ZonedTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance ToRow Message where
  toRow Message {..} = SQLite.toRow (content, author, subject, messageID, inReplyTo, references, ISO8601.iso8601Show date)

instance SQLite.FromRow Message where
  fromRow = do
    content <- SQLite.field
    author <- SQLite.field
    subject <- SQLite.field
    messageID <- SQLite.field
    inReplyTo <- SQLite.field
    references <- SQLite.field
    dateStr <- SQLite.field
    case ISO8601.iso8601ParseM dateStr of
      Just date -> return $ Message content author subject messageID inReplyTo references date
      Nothing -> case Time.parseTimeM True Time.defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q%z" dateStr of
        Just date -> return $ Message content author subject messageID inReplyTo references date
        Nothing -> error $ "Could not parse date: " ++ dateStr

preambleP :: Parser ()
preambleP = do
  Monad.void (Megaparsec.Char.string "From")
  Monad.void (Megaparsec.skipManyTill (Megaparsec.anySingleBut '\n') Megaparsec.Char.newline)

authorP :: Parser Text
authorP = do
  Monad.void (Megaparsec.Char.string "From: ")
  Monad.void (Megaparsec.skipMany (Megaparsec.anySingleBut '('))
  strName <-
    Megaparsec.between
      (Megaparsec.Char.char '(')
      (Megaparsec.Char.char ')')
      (Applicative.many (Megaparsec.anySingleBut ')'))
  -- let strName' = case Codec.MIME.Decode.decodeWord strName of
  --       Just (a, b) -> a <> " - " <> b
  --       Nothing -> strName
  Monad.void (Megaparsec.Char.newline)
  pure (decode strName)

decode :: String -> Text
decode = Text.Lazy.fromStrict <<< Data.MIME.EncodedWord.decodeEncodedWords Data.MIME.Charset.defaultCharsets <<< ByteString.Char8.pack

lineRemainderP :: Parser Text
lineRemainderP = do
  prefix <- singleLineRemainder
  rest <- Applicative.many (Megaparsec.Char.hspace1 *> singleLineRemainder)
  pure (Text.Lazy.intercalate " " (prefix : rest))
  where
    singleLineRemainder =
      decode <$> Megaparsec.someTill Megaparsec.anySingle Megaparsec.Char.newline

dateP :: Parser ZonedTime
dateP = do
  Monad.void (Megaparsec.Char.string "Date: ")
  remainder <- lineRemainderP
  case parseTimestamp (Text.Lazy.unpack remainder) of
    Just date -> pure date
    Nothing -> fail ("Could not parse date\n" <> show remainder)

subjectP :: Parser Text
subjectP = do
  Monad.void (Megaparsec.Char.string "Subject: ")
  lineRemainderP

inReplyToP :: Parser Text
inReplyToP = do
  Monad.void (Megaparsec.Char.string "In-Reply-To: ")
  lineRemainder <- lineRemainderP
  -- Sometimes this looks like
  --
  --   In-Reply-To: <ID> (So-and-so's message of "Some date")
  --
  -- and we want to extract only the ID.
  pure (Text.Lazy.takeWhile (/= ' ') lineRemainder)

referencesP :: Parser Text
referencesP = do
  Monad.void (Megaparsec.Char.string "References: ")
  lineRemainderP

messageIDP :: Parser Text
messageIDP = do
  Monad.void (Megaparsec.Char.string "Message-ID: ")
  lineRemainderP

contentP :: Parser Text
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
  -- TODO: The following doesn't really do anything I don't think
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
