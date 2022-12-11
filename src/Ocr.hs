module Ocr
    ( parseLetters
    ) where

import Control.Applicative
import Data.Maybe
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Debug.Trace

smallK :: String
smallK =
    unlines
    [ " ##  ###   ##  #### ####  ##  #  # ###   ## #  # #     ##  ###  ###   ### #  # #   # ####"
    , "#  # #  # #  # #    #    #  # #  #  #     # # #  #    #  # #  # #  # #    #  # #   #    #"
    , "#  # ###  #    ###  ###  #    ####  #     # ##   #    #  # #  # #  # #    #  #  # #    # "
    , "#### #  # #    #    #    # ## #  #  #     # # #  #    #  # ###  ###   ##  #  #   #    #  "
    , "#  # #  # #  # #    #    #  # #  #  #  #  # # #  #    #  # #    # #     # #  #   #   #   "
    , "#  # ###   ##  #### #     ### #  # ###  ##  #  # ####  ##  #    #  # ###   ##    #   ####"
    ]
smallV :: String
smallV = "ABCEFGHIJKLOPRSUYZ"

largeK :: String
largeK =
    unlines
    [ "  ##   #####   ####  ###### ######  ####  #    #    ### #    # #      #    # #####  #####  #    # ######"
    , " #  #  #    # #    # #      #      #    # #    #     #  #   #  #      ##   # #    # #    # #    #      #"
    , "#    # #    # #      #      #      #      #    #     #  #  #   #      ##   # #    # #    #  #  #       #"
    , "#    # #    # #      #      #      #      #    #     #  # #    #      # #  # #    # #    #  #  #      # "
    , "#    # #####  #      #####  #####  #      ######     #  ##     #      # #  # #####  #####    ##      #  "
    , "###### #    # #      #      #      #  ### #    #     #  ##     #      #  # # #      #  #     ##     #   "
    , "#    # #    # #      #      #      #    # #    #     #  # #    #      #  # # #      #   #   #  #   #    "
    , "#    # #    # #      #      #      #    # #    # #   #  #  #   #      #   ## #      #   #   #  #  #     "
    , "#    # #    # #    # #      #      #   ## #    # #   #  #   #  #      #   ## #      #    # #    # #     "
    , "#    # #####   ####  ###### #       ### # #    #  ###   #    # ###### #    # #      #    # #    # ######"
    ]
largeV :: String
largeV = "ABCEFGHJKLNPRXZ"

specialK :: String
specialK =
    unlines
    [ " ##        ##        ##    #    #  ####"
    , "#  #      #  #      #  #  # #  ##     #"
    , "#  #  ##  #            #  # #   #    # "
    , "#### #  # #           #   # #   #    # "
    , "#  # #  # #  #       #    # #   #   #  "
    , "#  #  ##   ##       ####   #   ###  #  "
    ]
specialV :: String
specialV = "AoC2017"

removeNewlines :: String -> String
removeNewlines = dropWhile (`elem` "\r\n") . foldr f []
    where f c [] = if c `elem` "\r\n" then [] else [c]
          f c s = c:s

separateLetters :: String -> [String]
separateLetters (removeNewlines -> input) = go (replicate (length lns) "") lns
    where lns = lines input
          ht (x:xs) = (x, xs)
          ht [] = error "0 length"
          go sofar lss
              | all null lss = if '#' `elem` letter then [letter] else []
              | any (=='#') col = go (zipWith (:) col sofar) lss'
              | '#' `elem` letter = letter : go (replicate (length lns) "") lss'
              | otherwise = go (replicate (length lns) "") lss'
              where (col, lss') = unzip $ map ht lss
                    letter = init $ unlines $ map reverse sofar

smallLetters :: HashMap String Char
smallLetters = M.fromList $ zip (separateLetters smallK) smallV

largeLetters :: HashMap String Char
largeLetters = M.fromList $ zip (separateLetters largeK) largeV

specialLetters :: HashMap String Char
specialLetters = M.fromList $ zip (separateLetters specialK) specialV

parseLetters :: String -> String
parseLetters = map get . separateLetters
    where get k = fromMaybe (error $ "Letter not found: " ++ k)
                  $ M.lookup k smallLetters
                  <|> M.lookup k largeLetters
                  <|> M.lookup k specialLetters
