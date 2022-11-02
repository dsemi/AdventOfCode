{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Utils (getProblemInput)

import Control.Lens ((^?))
import Control.Monad
import Data.Aeson
import Data.Aeson.Lens
import Data.String.Interpolate
import Days (problems)
import Data.Text (Text, pack)
import qualified Data.Text.IO as T
import Test.Hspec
import Text.Toml

validatePart :: Int -> Int -> Int -> Text -> (Text -> IO Text) -> Text -> Spec
validatePart year day p expected part input =
    describe [i|#{year} Day #{day} part #{p}|] $
             it "returns the correct answer for the problem input" $
                part input >>= (`shouldBe` expected)

tshow :: (Show a) => a -> Text
tshow = pack . show

validateDay :: Value -> Int -> Int -> (Text -> IO Text, Text -> IO Text) -> Spec
validateDay solns year day (part1, part2) = do
  let expect y d = (,) <$> (solns ^? key (tshow y) . key (tshow d) . key "part1" . _String) <*>
                     (solns ^? key (tshow y) . key (tshow d) . key "part2" . _String)
  case expect year day of
    Just (expected1, expected2) -> do
      input <- runIO $ getProblemInput year day False
      validatePart year day 1 expected1 part1 input
      validatePart year day 2 expected2 part2 input
    _ -> pure ()

readSolns :: IO Value
readSolns = do
  toml <- parseTomlDoc "" <$> T.readFile "test/expectedAnswers.toml"
  case toml of
    (Left _) -> error "error parsing solution file"
    (Right v) -> pure $ toJSON v

main :: IO ()
main = hspec $ do
         solns <- runIO $ readSolns
         forM_ problems $ \(year, days) ->
             forM_ days $ \(day, parts) ->
                 validateDay solns year day parts
