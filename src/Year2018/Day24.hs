{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}

module Year2018.Day24
    ( part1
    , part2
    ) where

import Control.Arrow
import Control.Lens
import Control.Monad.Extra
import Control.Monad.State
import Data.List (maximumBy, sortBy)
import Data.List.Split (splitOn)
import Data.Maybe
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M
import Data.IntSet (IntSet)
import qualified Data.IntSet as S
import Data.Ord
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer


data Group = Group { _name :: String
                   , _numUnits :: Int
                   , _hitpts :: Int
                   , _dmg :: Int
                   , _type' :: String
                   , _initiative :: Int
                   , _weaknesses :: [String]
                   , _immunities :: [String]
                   } deriving (Eq, Ord, Show)
makeLenses ''Group

type Battle = IntMap Group

effPwr :: Group -> Int
effPwr g = g^.numUnits * g^.dmg

parseGroups :: String -> Battle
parseGroups = M.fromList . zip [0..]
              . concatMap (fromJust . parseMaybe army) . splitOn "\n\n"
    where army = do
            n <- someTill anySingle (char ':') <* newline
            group n `sepBy` newline
          group :: String -> Parsec () String Group
          group n = do
            u <- decimal <* string " units each with "
            hp <- decimal <* string " hit points "
            ms <- fromMaybe []
                  <$> optional (between (char '(') (string ") ") (modifiers `sepBy` string "; "))
            d <- string "with an attack that does " *> decimal <* spaceChar
            t <- many letterChar <* string " damage at initiative "
            i <- decimal
            pure $ Group n u hp d t i (find "weak" ms) (find "immune" ms)
          find k = fromMaybe [] . lookup k
          modifiers = (,) <$> (some letterChar <* string " to ")
                      <*> (some letterChar `sepBy` string ", ")

calcDmg :: Group -> Group -> Int
calcDmg g1 g2
    | g1^.type' `elem` g2^.weaknesses = 2 * effPwr g1
    | g1^.type' `elem` g2^.immunities = 0
    | otherwise = effPwr g1

selectTarget :: (MonadState Battle m) => IntSet -> Group -> m (Maybe Int)
selectTarget attacked grp = do
  dmgs <- map (\(i, g) -> (i, calcDmg grp g, effPwr g, g^.initiative))
          . filter (\(i, g) -> grp^.name /= g^.name && S.notMember i attacked) . M.toList <$> get
  let max' = maximumBy (comparing (\(_, dm, pwr, ini) -> (dm, pwr, ini))) dmgs
  pure $ if null dmgs || view _2 max' == 0
         then Nothing
         else Just $ view _1 max'

targetSelection :: (MonadState Battle m) => m [(Int, Int)]
targetSelection = do
  groups <- get
  iniSort groups <$> go S.empty (targetSort (M.toList groups))
    where targetSort = sortBy (comparing (over both negate . (effPwr &&& _initiative) . snd))
          iniSort groups = sortBy (comparing (negate . _initiative . (groups M.!) . fst))
          go _ [] = pure []
          go s ((i, g) : rest) =
              selectTarget s g >>= \case
                           Just t -> ((i, t) :) <$> go (S.insert t s) rest
                           Nothing -> go s rest

attack :: (MonadState Battle m) => [(Int, Int)] -> m ()
attack atks = do
  forM_ atks $ \(k1, k2) -> do
    whenM (M.member k1 <$> get) $ do
      g1 <- fromJust <$> use (at k1)
      g2 <- fromJust <$> use (at k2)
      let unitsLeft = g2^.numUnits - calcDmg g1 g2 `div` g2^.hitpts
      if unitsLeft <= 0
      then modify' $ M.delete k2
      else modify' $ M.adjust (numUnits .~ unitsLeft) k2

battle :: (MonadState Battle m) => m Bool
battle = do
  s <- get
  let gps = M.elems s
  if any (head gps^.name /=) (map _name gps)
  then do
    targetSelection >>= attack
    ifM ((s /=) <$> get) battle (pure False)
  else pure True

part1 :: String -> Int
part1 = sum . map _numUnits . M.elems . execState battle . parseGroups

boostImmune :: Int -> Battle -> Battle
boostImmune n = M.map (\g -> if g^.name == "Immune System" then g & dmg +~ n else g)

part2 :: String -> Int
part2 input = let groups = parseGroups input
              in head $ [ sum' | n <- [0..]
                        , let (b, s) = runState battle $ boostImmune n groups
                        , let sum' = sum $ map _numUnits $ filter ((== "Immune System") . _name)
                                     $ M.elems s
                        , b && sum' > 0
                        ]
