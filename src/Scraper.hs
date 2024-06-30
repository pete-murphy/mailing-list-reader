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
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.Default qualified as Default
import Data.Text.Lazy (LazyText)
import Data.Text.Lazy.Encoding qualified as Text.Lazy.Encoding
import Network.Connection (TLSSettings (..))
import Network.HTTP.Client qualified as HTTP.Client
import Network.HTTP.Client.TLS qualified as HTTP.Client.TLS
import Network.HTTP.Types.Status qualified as HTTP.Types.Status
import Network.TLS qualified
import System.X509 qualified as X509

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
  year :: Int <- [2020 ..]
  month <- months
  let url = "https://mail.haskell.org/pipermail/haskell-cafe/" <> show year <> "-" <> month <> ".txt"
  pure url

type URL = String

fetchMessages :: URL -> IO LazyText
fetchMessages url = do
  putStrLn ("Fetching " <> url)
  request <- HTTP.Client.parseRequest url

  certificateStore <- X509.getSystemCertificateStore
  let tlsSettings =
        TLSSettings
          ( Network.TLS.defaultParamsClient
              (show (HTTP.Client.host request))
              (ByteString.Char8.pack (show (HTTP.Client.port request)))
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
  response <- HTTP.Client.httpLbs request manager
  let code =
        response
          & HTTP.Client.responseStatus
          & HTTP.Types.Status.statusCode
  Monad.when (code == 404) (fail "Doesn't exist")
  pure (response & HTTP.Client.responseBody & Text.Lazy.Encoding.decodeUtf8)

tryAll :: (Monad f, Alternative f) => [f a] -> f [a]
tryAll [] = pure []
tryAll (x : xs) =
  Applicative.optional x >>= \case
    Nothing -> return []
    Just a -> (a :) <$> tryAll xs

fetchAllMonths :: IO [LazyText]
fetchAllMonths = do
  tryAll (fetchMessages <$> mailingListLinks)
