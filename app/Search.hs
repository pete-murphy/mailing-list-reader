{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Search where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as TL
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
  -- Drop existing FTS table if it exists
  SQLite.execute_ conn "DROP TABLE IF EXISTS messages_fts"

  -- Create simpler FTS5 virtual table that stores its own content
  SQLite.execute_ conn $
    SQLite.Query $
      Text.unlines
        [ "CREATE VIRTUAL TABLE messages_fts USING fts5(",
          "  content, author, subject, message_id UNINDEXED,",
          "  in_reply_to UNINDEXED, references UNINDEXED, date UNINDEXED",
          ");"
        ]

-- Populate FTS table with existing data
populateSearchIndex :: SQLite.Connection -> IO ()
populateSearchIndex conn = do
  SQLite.execute_ conn $
    SQLite.Query $
      Text.unlines
        [ "INSERT INTO messages_fts(content, author, subject, message_id, in_reply_to, `references`, date)",
          "SELECT content, author, subject, message_id, in_reply_to, `references`, date FROM messages;"
        ]

-- Basic text search
searchMessages :: SQLite.Connection -> Text -> Int -> Int -> IO [SearchResult]
searchMessages conn query limit offset = do
  putStrLn $ "[LOG] Searching for: " ++ Text.unpack query ++ " (limit: " ++ show limit ++ ", offset: " ++ show offset ++ ")"
  putStrLn $ "[LOG] Raw query text: '" ++ Text.unpack query ++ "'"
  putStrLn $ "[LOG] SQL query: " ++ Text.unpack (SQLite.fromQuery searchQuery)

  -- First, let's try a simple subject search to see if we get results
  subjectResults <- SQLite.query conn subjectSearchQuery (query, limit, offset)
  putStrLn $ "[LOG] Subject-only search returned " ++ show (length subjectResults) ++ " results"

  -- Now try the FTS search
  results <- SQLite.query conn searchQuery (query, limit, offset)
  putStrLn $ "[LOG] FTS search returned " ++ show (length results) ++ " results"

  -- If FTS fails but subject search works, let's fall back to subject search
  if null results && not (null subjectResults)
    then do
      putStrLn "[LOG] FTS returned no results, falling back to subject search"
      return $ map (\msg -> SearchResult msg 1.0 (Text.take 200 (TL.toStrict (content msg)))) subjectResults
    else return results
  where
    searchQuery =
      SQLite.Query $
        Text.unlines
          [ "SELECT content, author, subject, message_id, in_reply_to, `references`, date,",
            "       bm25(messages_fts) as rank,",
            "       snippet(messages_fts, 0, '<mark>', '</mark>', '...', 32) as snippet",
            "FROM messages_fts",
            "WHERE messages_fts MATCH ?",
            "ORDER BY rank",
            "LIMIT ? OFFSET ?;"
          ]

    subjectSearchQuery =
      SQLite.Query $
        Text.unlines
          [ "SELECT content, author, subject, message_id, in_reply_to, `references`, date",
            "FROM messages",
            "WHERE subject LIKE ?",
            "ORDER BY date DESC",
            "LIMIT ? OFFSET ?;"
          ]

-- Search by author
searchByAuthor :: SQLite.Connection -> Text -> Int -> Int -> IO [Message]
searchByAuthor conn authorName limit offset = do
  putStrLn $ "[LOG] Searching by author: " ++ Text.unpack authorName ++ " (limit: " ++ show limit ++ ", offset: " ++ show offset ++ ")"
  messages <- SQLite.query conn authorQuery (authorName, limit, offset)
  putStrLn $ "[LOG] Database returned " ++ show (length messages) ++ " author messages"
  return messages
  where
    authorQuery =
      SQLite.Query $
        Text.unlines
          [ "SELECT content, author, subject, message_id, in_reply_to, `references`, date",
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
          [ "SELECT m.content, m.author, m.subject, m.message_id, m.in_reply_to, m.`references`, m.date,",
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
  putStrLn $ "[LOG] Getting thread context for message ID: " ++ Text.unpack messageId
  thread <- SQLite.query conn threadQuery (messageId, messageId)
  putStrLn $ "[LOG] Database returned " ++ show (length thread) ++ " thread messages"
  return thread
  where
    threadQuery =
      SQLite.Query $
        Text.unlines
          [ "SELECT content, author, subject, message_id, in_reply_to, `references`, date",
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
