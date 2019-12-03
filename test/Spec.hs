{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Lens ((^..))
import Control.Monad
import Data.Aeson.Lens
import Data.String.Interpolate
import Days (problems)
import Data.Text (pack)
import Test.Hspec


validate :: Int -> Int -> (String -> IO String, String -> IO String) -> Spec
validate year day (part1, part2) = do
  solns <- runIO $ readFile "test/expectedAnswers.json"
  let expect y d = solns ^.. key (pack $ show y) . key (pack $ show d) . values . _String
  case expect year day of
    [expected1, expected2] -> do
      input <- runIO $ readFile [i|inputs/#{year}/input#{day}.txt|]
      describe [i|#{year} Day #{day} part 1|] $
          it "returns the correct answer for the problem input" $
              fmap pack (part1 input) >>= (`shouldBe` expected1)
      describe [i|#{year} Day #{day} part 2|] $
          it "returns the correct answer for the problem input" $
              fmap pack (part2 input) >>= (`shouldBe` expected2)
    _ -> pure ()


main :: IO ()
main = hspec $ do
         forM_ problems $ \(year, days) ->
             forM_ days $ \(day, parts) ->
                 validate year day parts
