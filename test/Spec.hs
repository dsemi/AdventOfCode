{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Utils (getProblemInput)

import Control.Lens ((^?))
import Control.Monad
import Data.Aeson
import Data.Aeson.Key
import Data.Aeson.Lens
import Data.ByteString (ByteString)
import Data.String.Interpolate
import Days (problems)
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Test.Hspec
import Text.Toml

validatePart :: Int -> Int -> Int -> ByteString -> (ByteString -> IO ByteString) -> ByteString -> Spec
validatePart year day p expected part input =
    describe [i|#{year} Day #{day} part #{p}|] $
             it "returns the correct answer for the problem input" $
                part input >>= (`shouldBe` expected)

tkey :: (Show a) => a -> Key
tkey = fromString . show

validateDay :: Value -> Int -> Int -> (ByteString -> IO ByteString, ByteString -> IO ByteString) -> Spec
validateDay solns year day (part1, part2) = do
  let expect y d = (,) <$> (solns ^? key (tkey y) . key (tkey d) . key "part1" . _String) <*>
                     (solns ^? key (tkey y) . key (tkey d) . key "part2" . _String)
  case expect year day of
    Just (expected1, expected2) -> do
      input <- runIO $ getProblemInput year day False
      validatePart year day 1 (T.encodeUtf8 expected1) part1 input
      validatePart year day 2 (T.encodeUtf8 expected2) part2 input
    _ -> pure ()

readSolns :: IO Value
readSolns = do
  toml <- parseTomlDoc "" <$> T.readFile "test/expectedAnswers.toml"
  case toml of
    (Left _) -> error "error parsing solution file"
    (Right v) -> pure $ toJSON v

instance ToJSON Node where
    toJSON (VTable v)    = toJSON v
    toJSON (VTArray v)   = toJSON v
    toJSON (VString v)   = toJSON v
    toJSON (VInteger v)  = toJSON v
    toJSON (VFloat v)    = toJSON v
    toJSON (VBoolean v)  = toJSON v
    toJSON (VDatetime v) = toJSON v
    toJSON (VArray v)    = toJSON v

main :: IO ()
main = hspec $ do
         solns <- runIO $ readSolns
         forM_ problems $ \(year, days) ->
             forM_ days $ \(day, parts) ->
                 validateDay solns year day parts
