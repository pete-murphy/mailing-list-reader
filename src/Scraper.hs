{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Scraper where

import Control.Applicative (Alternative)
import Control.Applicative qualified as Applicative
import Control.Lens
import Control.Monad qualified as Monad
import Data.Text.Lazy (LazyText)
import Data.Text.Lazy.Encoding qualified as Text.Lazy.Encoding
import Network.HTTP.Client (Manager)
import Network.HTTP.Client qualified as HTTP.Client
import Network.HTTP.Types.Status qualified as HTTP.Types.Status

months :: [String]
months =
  [ "January",
    "February",
    "March",
    "April",
    "May",
    "June",
    "July",
    "August",
    "September",
    "October",
    "November",
    "December"
  ]

mailingListLinks :: [URL]
mailingListLinks = do
  year :: Int <- [2020 .. 2024]
  month <- months
  let url = "https://mail.haskell.org/pipermail/haskell-cafe/" <> show year <> "-" <> month <> ".txt"
  pure url

type URL = String

fetchMessages :: Int -> Manager -> URL -> IO LazyText
fetchMessages i manager url = do
  putStrLn ("[" <> show i <> "] Fetching " <> url)
  request <- HTTP.Client.parseRequest url
  response <- HTTP.Client.httpLbs request manager
  let code =
        response
          & HTTP.Client.responseStatus
          & HTTP.Types.Status.statusCode

  Monad.when (code == 404) (fail "Doesn't exist")
  let body = response & HTTP.Client.responseBody
  pure (Text.Lazy.Encoding.decodeLatin1 body)

tryAll :: (Monad f, Alternative f) => (a -> f b) -> [a] -> f [b]
tryAll _ [] = pure []
tryAll f (x : xs) =
  Applicative.optional (f x) >>= \case
    Nothing -> pure []
    Just a -> (a :) <$> tryAll f xs

-- fetchAllMonths :: Manager -> IO [LazyText]
-- fetchAllMonths manager = do
--   tryAll (fetchMessages manager) mailingListLinks
