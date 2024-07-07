{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Applicative qualified as Applicative
import Control.Arrow ((<<<))
import Control.Concurrent (MVar)
import Control.Concurrent qualified as MVar
import Control.Concurrent.Async qualified as Async
import Control.Lens (FunctorWithIndex (imap))
import Control.Monad qualified as Monad
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as ByteString.Lazy.Char8
import Data.Default qualified as Default
import Data.Foldable qualified as Foldable
import Data.Functor ((<&>))
import Data.Map qualified as Map
import Data.Text.Lazy qualified as Text.Lazy
import Data.Text.Lazy.Encoding qualified as Text.Lazy.Encoding
import Data.Text.Lazy.IO qualified as Text.Lazy.IO
import Data.Time qualified as Time
import Data.Time.Format.ISO8601 qualified as ISO8601
import Data.Traversable qualified as Traversable
import Debug.Trace qualified
import Network.Connection (TLSSettings (..))
import Network.HTTP.Client qualified as HTTP.Client
import Network.HTTP.Client.TLS qualified as HTTP.Client.TLS
import Network.TLS qualified
import Parser (Message (..))
import Parser qualified
import Pipes (Consumer, (>->))
import Pipes qualified
import Pipes.Concurrent qualified
import Pipes.Prelude qualified
import Scraper qualified
import System.IO (Handle, IOMode (..))
import System.IO qualified
import System.X509 qualified as X509
import Text.Megaparsec qualified as Megaparsec
import UnliftIO qualified

latin1 = "results-latin1.ndjson"

utf8 = "results.ndjson"

latest = "2024-07-06T04:43:30.531982Z-results-latin1.ndjson"

last = "2024-07-06T15:03:22.396222Z-results-latin1.ndjson"

main = do
  now <- takeWhile (/= '.') . ISO8601.iso8601Show <$> Time.getCurrentTime

  let thisOne = (now <> "-" <> latin1)

  runScrape thisOne

  -- runScrape utf8
  runCheck latest thisOne

-- runSampleContents latin1 [991, 820, 443, 554, 3839, 394, 2394, 239, 959, 2000, 1900, 1950]

runScrape :: FilePath -> IO ()
runScrape fileName = do
  certificateStore <- X509.getSystemCertificateStore
  let tlsSettings =
        TLSSettings
          ( Network.TLS.defaultParamsClient "https://mail.haskell.org" "443"
          )
            { Network.TLS.clientSupported =
                Default.def
                  { Network.TLS.supportedExtendedMainSecret = Network.TLS.AllowEMS
                  },
              Network.TLS.clientShared =
                Default.def
                  { Network.TLS.sharedCAStore = certificateStore
                  }
            }
  manager <- HTTP.Client.newManager (HTTP.Client.TLS.mkManagerSettings tlsSettings Nothing)

  System.IO.writeFile fileName ""
  System.IO.withFile fileName WriteMode \fileHandle -> do
    handleMVar <- MVar.newMVar fileHandle
    System.IO.hSetBuffering fileHandle System.IO.LineBuffering
    mailbox <- Pipes.Concurrent.spawn Pipes.Concurrent.unbounded

    consumers <-
      Traversable.for [1 .. 20] \i -> Async.async do
        Pipes.runEffect do
          Pipes.Concurrent.fromMailbox mailbox
            -- >-> Pipes.Prelude.tee
            --   ( Pipes.Prelude.map
            --       (\(message :: Message) -> show i <> ": " <> (Text.Lazy.unpack message.subject))
            --       >-> Pipes.Prelude.stdoutLn
            --   )
            >-> Pipes.Prelude.wither
              (Applicative.optional <<< (Scraper.fetchMessages i manager))
            >-> Pipes.Prelude.mapMaybe
              (hush <<< Megaparsec.runParser (Megaparsec.many Parser.messageP) "input")
            >-> Pipes.Prelude.concat
            >-> writeJSON handleMVar
        Pipes.Concurrent.performGC

    producer <-
      Async.async do
        Pipes.runEffect do
          Pipes.each Scraper.mailingListLinks
            -- >-> Pipes.Prelude.wither
            --   (Applicative.optional <<< (Scraper.fetchMessages manager))
            -- >-> Pipes.Prelude.mapMaybe
            --   (hush <<< Megaparsec.runParser (Megaparsec.many Parser.messageP) "input")
            -- >-> Pipes.Prelude.concat
            >-> Pipes.Concurrent.toMailbox mailbox
        Pipes.Concurrent.performGC

    Foldable.for_ (producer : consumers) Async.wait

hush :: Either e a -> Maybe a
hush = \case
  Left _ -> Nothing
  Right a -> Just a

writeJSON :: MVar Handle -> Consumer Message IO ()
writeJSON handleMVar = do
  Monad.forever do
    json <- Aeson.encode <$> Pipes.await
    Pipes.liftIO do
      MVar.withMVar handleMVar \handle -> do
        Pipes.liftIO (ByteString.Lazy.Char8.hPutStrLn handle json)
        Pipes.liftIO (System.IO.hFlush handle)

runCheck :: FilePath -> FilePath -> IO ()
runCheck f1 f2 = do
  l <-
    Text.Lazy.IO.readFile f1
      <&> Text.Lazy.lines
      <&> imap (\i x -> (i, Text.Lazy.toStrict x))
      <&> map (\(i, x) -> (i, x, (Aeson.eitherDecodeStrictText @Message x)))
      <&> map \case
        (_, _, Right m) -> (m.messageID, m.content)
        (i, ln, Left e) -> do
          Debug.Trace.traceM (show i)
          Debug.Trace.traceM (show ln)
          error (e)
      <&> Map.fromList
  u <-
    Text.Lazy.IO.readFile f2
      <&> Text.Lazy.lines
      <&> imap (\i x -> (i, Text.Lazy.toStrict x))
      <&> map (\(i, x) -> (i, x, (Aeson.eitherDecodeStrictText @Message x)))
      <&> map \case
        (_, _, Right m) -> (m.messageID, m.content)
        (i, ln, Left e) -> do
          Debug.Trace.traceM (show i)
          Debug.Trace.traceM (show ln)
          error (e)
      <&> Map.fromList
  Foldable.for_ (Map.toList l) \(k, v) -> do
    case Map.lookup k u of
      Just v' -> do
        -- putStrLn (show k)
        if v /= v'
          then do
            putStrLn "Different"
            putStrLn "--------------------------------------------||"
            Text.Lazy.IO.putStrLn v
            putStrLn "||------------------------------------------||"
            Text.Lazy.IO.putStrLn v'
            putStrLn "||--------------------------------------------"
          else
            pure ()
      Nothing -> do
        putStrLn "Missing"
        Text.Lazy.IO.putStrLn v

runSampleContents :: FilePath -> IO ()
runSampleContents fileName = do
  xs <-
    Text.Lazy.IO.readFile fileName
      <&> Text.Lazy.lines
      <&> imap (\i x -> (i, Text.Lazy.toStrict x))
      <&> map (\(i, x) -> (i, x, (Aeson.eitherDecodeStrictText @Message x)))
      <&> map \case
        (i, _, Right m) -> (i, m)
        (_, _, Left e) ->
          error (e)
  Foldable.for_ xs \(i, m) -> do
    -- if i `elem` ixs
    --   then do
    -- Text.Lazy.IO.putStrLn ("## " <> m.subject)
    -- Text.Lazy.IO.putStrLn (m.content)
    Text.Lazy.IO.putStrLn (m.date)

--
-- case Megaparsec.runParser (Applicative.many Parser.messageP) "input" text of
--   Left errBundle -> putStrLn (Megaparsec.errorBundlePretty errBundle)
--   Right result -> do
--     Foldable.for_ result \message -> do
--       Text.Lazy.IO.putStrLn (message ^. field @"content")
--       Text.Lazy.IO.putStrLn (message ^. field @"subject")
--       Text.Lazy.IO.putStrLn (message ^. field @"date")
--       Text.Lazy.IO.putStrLn (message ^. field @"messageID")
--       pure result

-- messages <-

--   Scraper.fetchAllMonths >>= foldMap \text -> do
--     case Megaparsec.runParser (Megaparsec.many Parser.messageP) "input" text of
--       Left errBundle -> do
--         putStrLn (Megaparsec.errorBundlePretty errBundle)
--         fail "An error occurred"
--       Right result -> do
--         Foldable.for_ result \message -> do
--           -- putStrLn "<========>"
--           -- Text.Lazy.IO.putStrLn (Parser.content message)
--           -- Text.Lazy.IO.putStrLn (message ^. field @"author")
--           Text.Lazy.IO.putStrLn (message ^. field @"subject")
--           -- Text.Lazy.IO.putStrLn (message ^. field @"inReplyTo" & maybe "" id)
--           -- Text.Lazy.IO.putStrLn (message ^. field @"references" & maybe "" id)
--           Text.Lazy.IO.putStrLn (message ^. field @"date")
--         -- Text.Lazy.IO.putStrLn (message ^. field @"messageID")
--         pure result
-- Aeson.encodeFile "messages-since-2010.json" messages
