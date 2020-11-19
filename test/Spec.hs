{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Utils (getProblemInput)

import Control.Lens ((^..))
import Control.Monad
import Data.Aeson.Lens
import Data.String.Interpolate
import Days (problems)
import Data.Text (Text, pack)
import Test.Hspec


validatePart :: Int -> Int -> Text -> (String -> IO String) -> String -> Spec
validatePart year day expected part input =
    describe [i|#{year} Day #{day} part 1|] $
             it "returns the correct answer for the problem input" $
                fmap pack (part input) >>= (`shouldBe` expected)


validateDay :: Int -> Int -> (String -> IO String, String -> IO String) -> Spec
validateDay year day (part1, part2) = do
  solns <- runIO $ readFile "test/expectedAnswers.json"
  let expect y d = solns ^.. key (pack $ show y) . key (pack $ show d) . values . _String
  case expect year day of
    [expected1, expected2] -> do
      input <- runIO $ getProblemInput year day
      validatePart year day expected1 part1 input
      validatePart year day expected2 part2 input
    _ -> pure ()


main :: IO ()
main = hspec $ do
         forM_ problems $ \(year, days) ->
             forM_ days $ \(day, parts) ->
                 validateDay year day parts
