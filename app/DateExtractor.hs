{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as B
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | Represents a single message, we only care about the date.
data Message = Message
  { date :: Text
  }
  deriving stock (Show, Generic)

instance Aeson.FromJSON Message where
  parseJSON = Aeson.genericParseJSON Aeson.defaultOptions

-- | Processes a single JSON file to extract dates.
processFile :: FilePath -> IO (Maybe (Text, [Text]))
processFile fp = do
  content <- B.readFile fp
  case Aeson.decode' content of
    Just (messages :: [Message]) -> do
      let dates = map date messages
      return $ Just (T.pack fp, dates)
    Nothing -> do
      putStrLn $ "Failed to parse " ++ fp
      return Nothing

main :: IO ()
main = do
  let files = ["messages-since-2010.json", "messages-since-2020.json"]

  results <- mapM processFile files

  let successfulResults = [(Key.fromText k, Aeson.toJSON v) | (k, v) <- catMaybes results]
      outputObject = Aeson.Object (KeyMap.fromList successfulResults)

  B.writeFile "dates.json" (Aeson.encode outputObject)
  putStrLn "Extracted dates to dates.json"