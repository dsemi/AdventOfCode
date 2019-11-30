{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Monad
import Control.Monad.State.Strict
import Data.Map (Map)
import qualified Data.Map as M
import Data.String.Interpolate
import Days (problems)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Test.Hspec


type Part m = Text -> m Text
type Problem m = (Part m , Part m)

validate :: Integer -> Integer -> Problem IO -> Spec
validate year day (part1, part2) = do
  input <- runIO $ T.readFile [i|inputs/#{year}/input#{day}.txt|]
  case expected year day of
    Just (expected1, expected2) -> do
      describe [i|#{year} Day #{day} part 1|] $
          it "returns the correct answer for the problem input" $
              part1 input >>= (`shouldBe` expected1)
      describe [i|#{year} Day #{day} part 2|] $
          it "returns the correct answer for the problem input" $
              part2 input >>= (`shouldBe` expected2)
    Nothing -> pure ()

key :: (Show a, Show b) => a -> b -> String
key y d = show y ++ "/" ++ show d

expected :: Integer -> Integer -> Maybe (Text, Text)
expected year day = M.lookup (key year day) ans
    where ans = flip execState M.empty expects

expect :: (MonadState (Map String (Text, Text)) m) => Int -> Int -> Text -> Text -> m ()
expect y d p1 p2 = modify' $ M.insert (key y d) ( p1, p2 )

expects :: (MonadState (Map String (Text, Text)) m) => m ()
expects = do
  expect 2015  1 "74" "1795"
  expect 2015  2 "1606483" "3842356"
  expect 2015  3 "2081" "2341"
  expect 2015  4 "117946" "3938038"
  expect 2015  5 "238" "69"
  expect 2015  6 "543903" "14687245"
  expect 2015  7 "46065" "14134"
  expect 2015  8 "1333" "2046"
  expect 2015  9 "207" "804"
  expect 2015 10 "252594" "3579328"
  expect 2015 11 "hepxxyzz" "heqaabcc"
  expect 2015 12 "156366" "96852"
  expect 2015 13 "664" "640"
  expect 2015 14 "2640" "1102"
  expect 2015 15 "21367368" "1766400"
  expect 2015 16 "373" "260"
  expect 2015 17 "1304" "18"
  expect 2015 18 "821" "886"
  expect 2015 19 "509" "195"
  expect 2015 20 "786240" "831600"
  expect 2015 21 "121" "201"
  expect 2015 22 "1824" "1937"
  expect 2015 23 "170" "247"
  expect 2015 24 "11266889531" "77387711"
  expect 2015 25 "8997277" ""

  expect 2016  1 "253" "126"
  expect 2016  2 "19636" "3CC43"
  expect 2016  3 "1032" "1838"
  expect 2016  4 "278221" "267"
  expect 2016  5 "d4cd2ee1" "f2c730e5"
  expect 2016  6 "afwlyyyq" "bhkzekao"
  expect 2016  7 "110" "242"
  expect 2016  8 "121" "\n###  #  # ###  #  #  ##  ####  ##  ####  ### #    \n#  # #  # #  # #  # #  # #    #  # #      #  #    \n#  # #  # #  # #  # #    ###  #  # ###    #  #    \n###  #  # ###  #  # #    #    #  # #      #  #    \n# #  #  # # #  #  # #  # #    #  # #      #  #    \n#  #  ##  #  #  ##   ##  ####  ##  ####  ### #### \n"
-- "RURUCEOEIL"
  expect 2016  9 "110346" "10774309173"
  expect 2016 10 "98" "4042"
  expect 2016 11 "37" "61"
  expect 2016 12 "318003" "9227657"
  expect 2016 13 "90" "135"
  expect 2016 14 "23769" "20606"
  expect 2016 15 "317371" "2080951"
  expect 2016 16 "10010101010011101" "01100111101101111"
  expect 2016 17 "RDRLDRDURD" "596"
  expect 2016 18 "2035" "20000577"
  expect 2016 19 "1834903" "1420280"
  expect 2016 20 "23923783" "125"
  expect 2016 21 "bgfacdeh" "bdgheacf"
  expect 2016 22 "955" "246"
  expect 2016 23 "11893" "479008453"
  expect 2016 24 "430" "700"
  expect 2016 25 "182" "\n      ##        ##        ##    #    #  ####      \n     #  #      #  #      #  #  # #  ##     #      \n     #  #  ##  #            #  # #   #    #       \n     #### #  # #           #   # #   #    #       \n     #  # #  # #  #       #    # #   #   #        \n     #  #  ##   ##       ####   #   ###  #        \n"
-- "AoC 2017"

  expect 2017  1 "1216" "1072"
  expect 2017  2 "41887" "226"
  expect 2017  3 "430" "312453"
  expect 2017  4 "477" "167"
  expect 2017  5 "336905" "21985262"
  expect 2017  6 "7864" "1695"
  expect 2017  7 "hlqnsbe" "1993"
  expect 2017  8 "2971" "4254"
  expect 2017  9 "16689" "7982"
  expect 2017 10 "1980" "899124dac21012ebc32e2f4d11eaec55"
  expect 2017 11 "759" "1501"
  expect 2017 12 "169" "179"
  expect 2017 13 "648" "3933124"
  expect 2017 14 "8250" "1113"
  expect 2017 15 "592" "320"
  expect 2017 16 "fnloekigdmpajchb" "amkjepdhifolgncb"
  expect 2017 17 "772" "42729050"
  expect 2017 18 "9423" "7620"
  expect 2017 19 "SXWAIBUZY" "16676"
  expect 2017 20 "258" "707"
  expect 2017 21 "160" "2271537"
  expect 2017 22 "5261" "2511927"
  expect 2017 23 "9409" "913"
  expect 2017 24 "1656" "1642"
  expect 2017 25 "2832" ""

  expect 2018  1 "406" "312"
  expect 2018  2 "8820" "bpacnmglhizqygfsjixtkwudr"
  expect 2018  3 "121259" "239"
  expect 2018  4 "119835" "12725"
  expect 2018  5 "10888" "6952"
  expect 2018  6 "4754" "42344"
  expect 2018  7 "DFOQPTELAYRVUMXHKWSGZBCJIN" "1036"
  expect 2018  8 "48155" "40292"
  expect 2018  9 "425688" "3526561003"
  expect 2018 10 "\n#    #  #       ######    ##    #    #  #####    ####   ######\n#    #  #            #   #  #   #   #   #    #  #    #       #\n #  #   #            #  #    #  #  #    #    #  #            #\n #  #   #           #   #    #  # #     #    #  #           # \n  ##    #          #    #    #  ##      #####   #          #  \n  ##    #         #     ######  ##      #    #  #  ###    #   \n #  #   #        #      #    #  # #     #    #  #    #   #    \n #  #   #       #       #    #  #  #    #    #  #    #  #     \n#    #  #       #       #    #  #   #   #    #  #   ##  #     \n#    #  ######  ######  #    #  #    #  #####    ### #  ######\n" "10656"
-- "XLZAKBGZ"
  expect 2018 11 "235,14" "237,227,14"
  expect 2018 12 "3605" "4050000000798"
  expect 2018 13 "102,114" "146,87"
  expect 2018 14 "2103141159" "20165733"
  expect 2018 15 "188576" "57112"
  expect 2018 16 "529" "573"
  expect 2018 17 "30635" "25094"
  expect 2018 18 "745008" "219425"
  expect 2018 19 "993" "10708912"
  expect 2018 20 "3046" "8545"
  expect 2018 21 "11513432" "7434231"
  expect 2018 22 "8681" "1070"
  expect 2018 23 "950" "86871407"
  expect 2018 24 "16086" "3957"
  expect 2018 25 "318" ""


main :: IO ()
main = hspec $ do
         forM_ problems $ \(year, days) ->
             forM_ days $ \(day, parts) ->
                 validate year day parts
