{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec where

import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy.IO qualified as Text.Lazy.IO
import Parser qualified
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec qualified as Megaparsec

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "tests"
    [ testCase "1" do
        txt <- liftIO (Text.Lazy.IO.readFile "./test/2020-January.txt")
        c0 <- liftIO (Text.Lazy.IO.readFile "./test/2020-January-c0.txt")
        c1 <- liftIO (Text.Lazy.IO.readFile "./test/2020-January-c1.txt")
        case Megaparsec.runParser (Megaparsec.many Parser.messageP) "input" txt of
          Left e -> assertFailure (show e)
          Right messages -> do
            messages ^. ix 0 . #content @?= c0
            messages ^. ix 1 . #content @?= c1

            take 5 (messages <&> (^. #author))
              @?= [ "Samir Genaim",
                    "Rob Stewart",
                    "Debasish Ghosh",
                    "Tom Ellis",
                    "Viktor Dukhovni"
                  ]
    ]
