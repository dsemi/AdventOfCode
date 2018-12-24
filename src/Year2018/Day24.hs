{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}

module Year2018.Day24
    ( part1
    , part2
    ) where

import Control.Arrow
import Control.Lens
import Control.Monad.State
import Data.List (maximumBy, partition, sortBy)
import Data.List.Split (splitOn)
import Data.Maybe
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Ord
import Data.Tuple
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Debug.Trace


type Key = Int
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

data Battle = Battle { _attacks :: [(Key, Key)]
                     , _groups :: Map Key Group
                     } deriving (Eq, Show)
makeLenses ''Battle

effPwr :: Group -> Int
effPwr g = g^.numUnits * g^.dmg

parseGroups :: String -> Map Key Group
parseGroups = M.fromList . zip [0..]
              . concatMap (fromJust . parseMaybe army) . splitOn "\n\n"
    where army = do
            n <- someTill anyChar (char ':') <* newline
            group n `sepBy` newline
          group :: String -> Parsec () String Group
          group n = do
            u <- decimal <* string " units each with "
            hp <- decimal <* string " hit points "
            (ws, is) <- fromMaybe ([], []) <$> optional (between (char '(') (string ") ") wAndI)
            d <- string "with an attack that does " *> decimal <* spaceChar
            t <- many letterChar <* string " damage at initiative "
            i <- decimal
            pure $ Group n u hp d t i ws is
          weakness = string "weak to " *> some letterChar `sepBy` string ", "
          immunity = string "immune to " *> some letterChar `sepBy` string ", "
          wAndI = try ((,) <$> weakness <*> (string "; " *> immunity))
                  <|> try (swap <$> ((,) <$> immunity <*> (string "; " *> weakness)))
                  <|> try ((,[]) <$> weakness)
                  <|> try (([],) <$> immunity)

calcDmg :: Group -> Group -> Int
calcDmg g1 g2
    | g1^.type' `elem` g2^.weaknesses = 2 * effPwr g1
    | g1^.type' `elem` g2^.immunities = 0
    | otherwise = effPwr g1

selectTarget :: (MonadState Battle m) => Group -> m (Maybe Key)
selectTarget grp = do
  attacked <- map snd <$> use attacks
  poss <- filter (\(i, g) -> grp^.name /= g^.name && i `notElem` attacked) . M.toList <$> use groups
  let dmgs = map (\(i, g) -> (i, calcDmg grp g, effPwr g, g^.initiative)) poss
      max' = maximumBy (comparing (\(_, dm, pwr, ini) -> (dm, pwr, ini))) dmgs
  if null poss || view _2 max' == 0
  then pure Nothing
  else pure $ Just $ view _1 max'

targetSelection :: (MonadState Battle m) => m ()
targetSelection = do
  gs <- sortBy (comparing (over both negate . (effPwr &&& _initiative) . snd))
        . M.toList <$> use groups
  forM_ gs $ \(i, g) -> do
    t <- selectTarget g
    when (isJust t) $ attacks %= ((i, fromJust t):)
  gps <- use groups
  attacks %= sortBy (comparing (negate . _initiative . (gps M.!) . fst))
  pure ()

attack :: (MonadState Battle m) => m ()
attack = do
  atks <- use attacks
  forM_ atks $ \(k1, k2) -> do
    gps <- use groups
    when (M.member k1 gps) $ do
      let g1 = gps M.! k1
          g2 = gps M.! k2
          d = calcDmg g1 g2
      if d > g2^.hitpts * g2^.numUnits
      then groups %= M.delete k2
      else groups %= M.adjust (numUnits %~ (subtract (d `div` g2^.hitpts))) k2
  attacks .= []

battle :: (MonadState Battle m) => m ()
battle = get >>= go
    where go s = do
            gps <- M.elems <$> use groups
            when (not (all (head gps^.name ==) (map _name gps))) $ do
              targetSelection
              attack
              s' <- get
              when (s /= s') $ go s'

numUnitsLeft :: Map Key Group -> Int
numUnitsLeft = sum . map _numUnits . M.elems . _groups . execState battle . Battle []

part1 :: String -> Int
part1 = numUnitsLeft . parseGroups

boostImmune :: Int -> Map Key Group -> Map Key Group
boostImmune n = M.map (\g -> if g^.name == "Immune System" then g & dmg +~ n else g)

part2 :: String -> Int
part2 input = let gps = parseGroups input
              in head $ filter (>0)
                     $ map (\n -> let (imms, infs) = partition ((== "Immune System") . _name)
                                                     $ M.elems $ _groups $ execState battle
                                                     $ Battle [] $ boostImmune n gps
                                      ns = sum $ map _numUnits imms
                                  in if null infs then ns else 0) [0..]
