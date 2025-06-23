{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Search where

import Data.Text (Text)
import Data.Text qualified as Text
import Database.SQLite.Simple qualified as SQLite
import Database.SQLite.Simple.FromRow qualified as SQLite
import Parser (Message (..))

-- Search result with relevance ranking
data SearchResult = SearchResult
  { message :: Message,
    rank :: Double,
    snippet :: Text
  }
  deriving (Show)

instance SQLite.FromRow SearchResult where
  fromRow =
    SearchResult
      <$> SQLite.fromRow
      <*> SQLite.field -- rank
      <*> SQLite.field -- snippet

-- Initialize FTS5 virtual table for full-text search
initializeSearchIndex :: SQLite.Connection -> IO ()
initializeSearchIndex conn = do
  -- Create FTS5 virtual table
  SQLite.execute_ conn $
    SQLite.Query $
      Text.unlines
        [ "CREATE VIRTUAL TABLE IF NOT EXISTS messages_fts USING fts5(",
          "  content, author, subject, message_id UNINDEXED,",
          "  in_reply_to UNINDEXED, date UNINDEXED,",
          "  content='messages', content_rowid='id'",
          ");"
        ]

  -- Create triggers to keep FTS in sync
  SQLite.execute_ conn $
    SQLite.Query $
      Text.unlines
        [ "CREATE TRIGGER IF NOT EXISTS messages_ai AFTER INSERT ON messages BEGIN",
          "  INSERT INTO messages_fts(rowid, content, author, subject, message_id, in_reply_to, date)",
          "  VALUES (new.id, new.content, new.author, new.subject, new.message_id, new.in_reply_to, new.date);",
          "END;"
        ]

  SQLite.execute_ conn $
    SQLite.Query $
      Text.unlines
        [ "CREATE TRIGGER IF NOT EXISTS messages_ad AFTER DELETE ON messages BEGIN",
          "  INSERT INTO messages_fts(messages_fts, rowid, content, author, subject, message_id, in_reply_to, date)",
          "  VALUES('delete', old.id, old.content, old.author, old.subject, old.message_id, old.in_reply_to, old.date);",
          "END;"
        ]

-- Populate FTS table with existing data
populateSearchIndex :: SQLite.Connection -> IO ()
populateSearchIndex conn = do
  SQLite.execute_ conn $
    SQLite.Query $
      Text.unlines
        [ "INSERT INTO messages_fts(rowid, content, author, subject, message_id, in_reply_to, date)",
          "SELECT id, content, author, subject, message_id, in_reply_to, date FROM messages;"
        ]

-- Basic text search
searchMessages :: SQLite.Connection -> Text -> Int -> Int -> IO [SearchResult]
searchMessages conn query limit offset = do
  SQLite.query conn searchQuery (query, limit, offset)
  where
    searchQuery =
      SQLite.Query $
        Text.unlines
          [ "SELECT m.content, m.author, m.subject, m.message_id, m.in_reply_to, m.references, m.date,",
            "       bm25(messages_fts) as rank,",
            "       snippet(messages_fts, 0, '<mark>', '</mark>', '...', 32) as snippet",
            "FROM messages_fts",
            "JOIN messages m ON m.id = messages_fts.rowid",
            "WHERE messages_fts MATCH ?",
            "ORDER BY rank",
            "LIMIT ? OFFSET ?;"
          ]

-- Search by author
searchByAuthor :: SQLite.Connection -> Text -> Int -> Int -> IO [Message]
searchByAuthor conn author limit offset = do
  SQLite.query conn authorQuery (author, limit, offset)
  where
    authorQuery =
      SQLite.Query $
        Text.unlines
          [ "SELECT content, author, subject, message_id, in_reply_to, references, date",
            "FROM messages",
            "WHERE author LIKE ?",
            "ORDER BY date DESC",
            "LIMIT ? OFFSET ?;"
          ]

-- Search within date range
searchByDateRange :: SQLite.Connection -> Text -> Text -> Text -> Int -> Int -> IO [SearchResult]
searchByDateRange conn query startDate endDate limit offset = do
  SQLite.query conn dateRangeQuery (query, startDate, endDate, limit, offset)
  where
    dateRangeQuery =
      SQLite.Query $
        Text.unlines
          [ "SELECT m.content, m.author, m.subject, m.message_id, m.in_reply_to, m.references, m.date,",
            "       bm25(messages_fts) as rank,",
            "       snippet(messages_fts, 0, '<mark>', '</mark>', '...', 32) as snippet",
            "FROM messages_fts",
            "JOIN messages m ON m.id = messages_fts.rowid",
            "WHERE messages_fts MATCH ? AND date >= ? AND date <= ?",
            "ORDER BY rank",
            "LIMIT ? OFFSET ?;"
          ]

-- Get thread context (replies to a message)
getThreadContext :: SQLite.Connection -> Text -> IO [Message]
getThreadContext conn messageId = do
  SQLite.query conn threadQuery (messageId, messageId)
  where
    threadQuery =
      SQLite.Query $
        Text.unlines
          [ "SELECT content, author, subject, message_id, in_reply_to, references, date",
            "FROM messages",
            "WHERE in_reply_to = ? OR message_id = ?",
            "ORDER BY date;"
          ]

-- Get popular search terms (most discussed topics)
getPopularTopics :: SQLite.Connection -> Int -> IO [(Text, Int)]
getPopularTopics conn limit = do
  SQLite.query conn popularQuery (SQLite.Only limit)
  where
    popularQuery =
      SQLite.Query $
        Text.unlines
          [ "SELECT TRIM(REPLACE(REPLACE(subject, '[Haskell-cafe]', ''), 'Re:', '')) as topic,",
            "       COUNT(*) as count",
            "FROM messages",
            "WHERE LENGTH(TRIM(REPLACE(REPLACE(subject, '[Haskell-cafe]', ''), 'Re:', ''))) > 0",
            "GROUP BY topic",
            "ORDER BY count DESC",
            "LIMIT ?;"
          ]
