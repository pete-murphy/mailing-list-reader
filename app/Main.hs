{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Applicative qualified as Applicative
import Control.Arrow ((<<<))
import Control.Concurrent (MVar)
import Control.Concurrent qualified as MVar
import Control.Concurrent.Async qualified as Async
import Control.Lens qualified
import Control.Monad qualified as Monad
import Data.Attoparsec.ByteString qualified as Attoparsec.ByteString
import Data.ByteString (StrictByteString)
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.Default qualified as Default
import Data.Foldable qualified as Foldable
import Data.IMF (BodyHandler (..))
import Data.IMF qualified as IMF
import Data.MIME qualified as MIME
import Data.Text.IO qualified as Text.IO
import Data.Text.Lazy qualified as Text.Lazy
import Data.Time qualified as Time
import Data.Time.Format.ISO8601 qualified as ISO8601
import Data.Traversable qualified as Traversable
import Database.SQLite.Simple qualified as SQLite
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
import Search qualified
import System.Environment qualified as Environment
import System.IO qualified
import System.X509 qualified as X509
import Text.Megaparsec qualified as Megaparsec
import WebServer qualified

-- SQLite database setup
initializeDatabase :: FilePath -> IO SQLite.Connection
initializeDatabase dbPath = do
  conn <- SQLite.open dbPath
  SQLite.execute_ conn $
    SQLite.Query $
      Text.Lazy.toStrict $
        Text.Lazy.unlines
          [ "CREATE TABLE IF NOT EXISTS messages (",
            "  id INTEGER PRIMARY KEY AUTOINCREMENT,",
            "  content TEXT NOT NULL,",
            "  author TEXT NOT NULL,",
            "  subject TEXT NOT NULL,",
            "  message_id TEXT UNIQUE NOT NULL,",
            "  in_reply_to TEXT,",
            "  `references` TEXT,",
            "  date TEXT NOT NULL",
            ")"
          ]
  pure conn

main :: IO ()
main = do
  args <- Environment.getArgs
  case args of
    ["scrape"] -> do
      now <- takeWhile (/= '.') . ISO8601.iso8601Show <$> Time.getCurrentTime
      let dbPath = now <> "-messages.db"
      runScrape dbPath
    ["search", dbPath] -> runSearch dbPath
    ["web", dbPath] -> runWebServer dbPath
    ["index", dbPath] -> runIndexing dbPath
    _ -> do
      putStrLn "Usage:"
      putStrLn "  mailing-list-reader scrape                    # Scrape new messages"
      putStrLn "  mailing-list-reader search <db-file>          # Interactive search"
      putStrLn "  mailing-list-reader web <db-file>             # Start web server"
      putStrLn "  mailing-list-reader index <db-file>           # Build search index"

analyse :: StrictByteString -> IO ()
analyse input = do
  putStrLn "Analyse"
  case IMF.parse (Applicative.some (IMF.message (\_ -> RequiredBody Attoparsec.ByteString.takeByteString))) input of
    Left err -> do
      putStrLn "Error"
      putStrLn err
      System.IO.hPutStrLn System.IO.stderr err
    Right msgs -> do
      putStrLn "Success"
      print (Foldable.length msgs)
      Foldable.for_ msgs \msg -> do
        Text.IO.putStrLn ("\n\nsubject:\n\n" <> Control.Lens.foldOf (IMF.headerSubject MIME.defaultCharsets . Control.Lens._Just) msg)
        ByteString.Char8.putStrLn ("\n\nbody:\n\n" <> Control.Lens.view IMF.body msg)

runScrape :: FilePath -> IO ()
runScrape dbPath = do
  certificateStore <- X509.getSystemCertificateStore
  let clientParams =
        (Network.TLS.defaultParamsClient "https://mail.haskell.org" "443")
          { Network.TLS.clientSupported =
              Default.def {Network.TLS.supportedExtendedMainSecret = Network.TLS.AllowEMS},
            Network.TLS.clientShared =
              Default.def {Network.TLS.sharedCAStore = certificateStore}
          }
      tlsSettings = TLSSettings clientParams

  manager <- HTTP.Client.newManager (HTTP.Client.TLS.mkManagerSettings tlsSettings Nothing)

  conn <- initializeDatabase dbPath
  connMVar <- MVar.newMVar conn
  mailbox <- Pipes.Concurrent.spawn Pipes.Concurrent.unbounded

  consumers <-
    Traversable.for [1 .. 20] \i -> Async.async do
      Pipes.runEffect do
        Pipes.Concurrent.fromMailbox mailbox
          >-> Pipes.Prelude.wither
            (Applicative.optional <<< Scraper.fetchMessages i manager)
          >-> Pipes.Prelude.mapMaybe
            (hush <<< Megaparsec.runParser (Megaparsec.many Parser.messageP) "input")
          >-> Pipes.Prelude.concat
          >-> writeDB connMVar
      Pipes.Concurrent.performGC

  producer <-
    Async.async do
      Pipes.runEffect do
        Pipes.each Scraper.mailingListLinks
          >-> Pipes.Concurrent.toMailbox mailbox
      Pipes.Concurrent.performGC

  Foldable.for_ (producer : consumers) Async.wait
  SQLite.close conn

-- Build search index for existing database
runIndexing :: FilePath -> IO ()
runIndexing dbPath = do
  putStrLn $ "Building search index for " <> dbPath
  conn <- SQLite.open dbPath
  Search.initializeSearchIndex conn
  Search.populateSearchIndex conn
  SQLite.close conn
  putStrLn "Search index built successfully!"

-- Interactive search mode
runSearch :: FilePath -> IO ()
runSearch dbPath = do
  conn <- SQLite.open dbPath
  Search.initializeSearchIndex conn -- Ensure FTS tables exist
  putStrLn "=== Haskell Mailing List Search ==="
  putStrLn "Enter search queries (or 'quit' to exit):"
  searchLoop conn
  SQLite.close conn
  where
    searchLoop conn = do
      putStr "Search> "
      System.IO.hFlush System.IO.stdout
      query <- getLine
      if query == "quit"
        then putStrLn "Goodbye!"
        else do
          results <- Search.searchMessages conn (Text.Lazy.toStrict $ Text.Lazy.pack query) 5 0
          if null results
            then putStrLn "No results found."
            else do
              putStrLn $ "\nFound " <> show (length results) <> " results:\n"
              Foldable.for_ (zip [1 ..] results) \(i, result) -> do
                putStrLn $ show i <> ". " <> Text.Lazy.unpack (Text.Lazy.fromStrict result . message . subject)
                putStrLn $ "   By: " <> Text.Lazy.unpack (Text.Lazy.fromStrict result . message . author)
                putStrLn $ "   Date: " <> Text.Lazy.unpack (Text.Lazy.fromStrict $ Text.take 10 result . message . date)
                putStrLn $ "   " <> Text.Lazy.unpack (Text.Lazy.fromStrict $ Text.take 100 result . snippet) <> "..."
                putStrLn ""
          searchLoop conn

-- Web server mode
runWebServer :: FilePath -> IO ()
runWebServer dbPath = do
  conn <- SQLite.open dbPath
  Search.initializeSearchIndex conn -- Ensure FTS tables exist
  WebServer.startServer conn 8080
