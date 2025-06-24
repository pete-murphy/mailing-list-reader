{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module WebServer where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as Text.Lazy
import Data.Time.Format.ISO8601 qualified as ISO8601
import Database.SQLite.Simple qualified as SQLite
import Network.Wai.Handler.Warp (run)
import Parser (Message (..))
import Search qualified
import Servant
import Servant.HTML.Blaze
import Text.Blaze ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

-- API definition
type SearchAPI =
  Get '[HTML] H.Html
    :<|> "search"
      :> QueryParam "q" Text
      :> QueryParam "page" Int
      :> Get '[HTML] H.Html
    :<|> "author"
      :> Capture "author" Text
      :> QueryParam "page" Int
      :> Get '[HTML] H.Html
    :<|> "thread"
      :> Capture "messageId" Text
      :> Get '[HTML] H.Html
    :<|> "static" :> Raw

searchAPI :: Proxy SearchAPI
searchAPI = Proxy

-- Server implementation
server :: SQLite.Connection -> Server SearchAPI
server conn =
  homePage
    :<|> searchPage
    :<|> authorPage
    :<|> threadPage
    :<|> serveDirectoryWebApp "static"
  where
    homePage :: Handler H.Html
    homePage = liftIO $ do
      topics <- Search.getPopularTopics conn 20
      return $ pageTemplate "Haskell Mailing List Search" $ do
        H.div ! A.class_ "hero" $ do
          H.h1 "Haskell Mailing List Archive"
          H.p "Search through 47,000+ messages from 24 years of Haskell discussions"

        searchForm Nothing

        H.div ! A.class_ "popular-topics" $ do
          H.h2 "Popular Discussion Topics"
          H.ul $ mapM_ topicItem topics
      where
        topicItem (topic, count) = H.li $ do
          H.a ! A.href (H.textValue $ "/search?q=" <> topic) $ H.toHtml topic
          H.span ! A.class_ "count" $ H.toHtml $ " (" <> Text.pack (show count) <> " messages)"

    searchPage :: Maybe Text -> Maybe Int -> Handler H.Html
    searchPage Nothing _ = homePage
    searchPage (Just query) maybePage = liftIO $ do
      let page = maybe 0 Prelude.id maybePage
          offset = page * 20
      results <- Search.searchMessages conn query 20 offset
      return $ pageTemplate ("Search: " <> query) $ do
        searchForm (Just query)

        H.div ! A.class_ "results" $ do
          H.h2 $ H.toHtml $ "Search results for: " <> query
          mapM_ resultItem results

        pagination query page (length results == 20)
      where
        resultItem result = H.div ! A.class_ "result" $ do
          H.div ! A.class_ "meta" $ do
            H.strong $ H.toHtml (Text.Lazy.toStrict result.message.author)
            H.span " • "
            H.span $ H.toHtml (Text.take 19 (Text.pack (ISO8601.iso8601Show result.message.date)))
            H.span " • "
            H.a ! A.href (H.textValue $ "/thread/" <> Text.Lazy.toStrict result.message.messageID) $ "View Thread"
          H.h3 $ H.toHtml result.message.subject
          H.div ! A.class_ "snippet" $ H.preEscapedToHtml result.snippet

    authorPage :: Text -> Maybe Int -> Handler H.Html
    authorPage author maybePage = liftIO $ do
      let page = maybe 0 Prelude.id maybePage
          offset = page * 20
      messages <- Search.searchByAuthor conn ("%" <> author <> "%") 20 offset
      return $ pageTemplate ("Messages by " <> author) $ do
        H.h1 $ H.toHtml $ "Messages by " <> author
        H.div ! A.class_ "messages" $ mapM_ messageItem messages
        authorPagination author page (length messages == 20)
      where
        messageItem message = H.div ! A.class_ "message" $ do
          H.div ! A.class_ "meta" $ do
            H.span $ H.toHtml (Text.take 19 (Text.pack (ISO8601.iso8601Show message.date)))
            H.span " • "
            (H.a ! A.href (H.textValue ("/thread/" <> Text.Lazy.toStrict message.messageID))) "View Thread"
          H.h3 $ H.toHtml message.subject
          H.p $ H.toHtml $ Text.take 200 (Text.Lazy.toStrict message.content) <> "..."

    threadPage :: Text -> Handler H.Html
    threadPage messageId = liftIO $ do
      thread <- Search.getThreadContext conn messageId
      return $ pageTemplate "Thread View" $ do
        H.h1 "Thread Context"
        H.div ! A.class_ "thread" $ mapM_ threadMessage thread
      where
        threadMessage message = H.div ! A.class_ "thread-message" $ do
          H.div ! A.class_ "meta" $ do
            H.strong $ H.toHtml message.author
            H.span " • "
            H.span $ H.toHtml (Text.pack $ ISO8601.iso8601Show message.date)
          H.h3 $ H.toHtml message.subject
          H.pre $ H.toHtml message.content

-- HTML helpers
pageTemplate :: Text -> H.Html -> H.Html
pageTemplate title content = H.docTypeHtml $ do
  H.head $ do
    H.title $ H.toHtml title
    H.meta ! A.charset "utf-8"
    H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
    H.style $ H.toHtml cssStyles
  H.body $ do
    H.div ! A.class_ "container" $ content

searchForm :: Maybe Text -> H.Html
searchForm maybeQuery = H.form ! A.method "GET" ! A.action "/search" ! A.class_ "search-form" $ do
  H.input
    ! A.type_ "text"
    ! A.name "q"
    ! A.placeholder "Search messages..."
    ! A.value (maybe "" H.textValue maybeQuery)
    ! A.class_ "search-input"
  H.input ! A.type_ "submit" ! A.value "Search" ! A.class_ "search-button"

pagination :: Text -> Int -> Bool -> H.Html
pagination query page hasNext = H.div ! A.class_ "pagination" $ do
  when (page > 0) $
    H.a ! A.href (H.textValue $ "/search?q=" <> query <> "&page=" <> Text.pack (show (page - 1))) $
      "← Previous"
  when hasNext $
    H.a ! A.href (H.textValue $ "/search?q=" <> query <> "&page=" <> Text.pack (show (page + 1))) $
      "Next →"

authorPagination :: Text -> Int -> Bool -> H.Html
authorPagination author page hasNext = H.div ! A.class_ "pagination" $ do
  when (page > 0) $
    H.a ! A.href (H.textValue $ "/author/" <> author <> "?page=" <> Text.pack (show (page - 1))) $
      "← Previous"
  when hasNext $
    H.a ! A.href (H.textValue $ "/author/" <> author <> "?page=" <> Text.pack (show (page + 1))) $
      "Next →"

-- Basic CSS
cssStyles :: String
cssStyles =
  unlines
    [ "body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; margin: 0; padding: 20px; background: #f5f5f5; }",
      ".container { max-width: 1200px; margin: 0 auto; background: white; padding: 40px; border-radius: 8px; box-shadow: 0 2px 10px rgba(0,0,0,0.1); }",
      ".hero { text-align: center; margin-bottom: 40px; }",
      ".hero h1 { color: #333; margin-bottom: 10px; }",
      ".hero p { color: #666; font-size: 18px; }",
      ".search-form { display: flex; gap: 10px; margin: 30px 0; justify-content: center; }",
      ".search-input { padding: 12px; font-size: 16px; border: 2px solid #ddd; border-radius: 6px; width: 400px; }",
      ".search-button { padding: 12px 24px; background: #007acc; color: white; border: none; border-radius: 6px; cursor: pointer; }",
      ".search-button:hover { background: #005999; }",
      ".result, .message, .thread-message { border-bottom: 1px solid #eee; padding: 20px 0; }",
      ".meta { color: #666; margin-bottom: 10px; }",
      ".meta a { color: #007acc; text-decoration: none; }",
      ".snippet { background: #f9f9f9; padding: 15px; border-radius: 4px; margin-top: 10px; }",
      ".snippet mark { background: #ffeb3b; padding: 2px 4px; }",
      ".popular-topics ul { list-style: none; padding: 0; columns: 2; }",
      ".popular-topics li { margin: 10px 0; }",
      ".count { color: #999; font-size: 14px; }",
      ".pagination { margin: 30px 0; text-align: center; }",
      ".pagination a { margin: 0 10px; padding: 10px 20px; background: #007acc; color: white; text-decoration: none; border-radius: 4px; }",
      "pre { white-space: pre-wrap; background: #f5f5f5; padding: 15px; border-radius: 4px; overflow-x: auto; }"
    ]

-- Start the web server
startServer :: SQLite.Connection -> Int -> IO ()
startServer conn port = do
  putStrLn $ "Starting server on http://localhost:" ++ show port
  run port (serve searchAPI (server conn))
