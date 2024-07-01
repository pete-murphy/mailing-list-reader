{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Data.Aeson qualified as Aeson
import Parser qualified
import Scraper qualified
import Text.Megaparsec qualified as Megaparsec

main :: IO ()
main = do
  messages <-
    Scraper.fetchAllMonths >>= foldMap \text -> do
      case Megaparsec.runParser (Megaparsec.many Parser.messageP) "input" text of
        Left errBundle -> do
          putStrLn (Megaparsec.errorBundlePretty errBundle)
          fail "An error occurred"
        Right result -> pure result
  Aeson.encodeFile "messages.json" messages
