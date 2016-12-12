module Year2016.Day10
    ( part1
    , part2
    ) where

import Data.Coerce
import Control.Monad.State
import Data.List (sort)
import Data.Map.Strict ((!), Map)
import qualified Data.Map.Strict as M

data BotInputs = None | One Int | Two Int Int deriving (Eq)
newtype BotId = BotId Int deriving (Eq, Ord)
newtype OutId = OutId Int deriving (Eq, Ord)
data Target = B BotId | O OutId deriving (Eq, Ord)
data Targets = Targets Target Target

data Node = Bot { bId :: BotId
                , botFunc :: Maybe ActiveBot
                , inputs :: BotInputs
                , targets :: Targets
                }
          | Output { oId :: OutId
                   , input :: Int
                   }

data ActiveBot = ActiveBot (Int -> State (Map Target Node) ())

botStage1 :: BotId -> Targets -> Int -> State (Map Target Node) ()
botStage1 n targets@(Targets t1 t2) v1 =
    modify $ M.insert (B n) (Bot n (Just (ActiveBot botStage2)) (One v1) targets)
    where botStage2 v2 = do
            let xs@[i1, i2] = sort [v1, v2]
            modify $ M.insert (B n) (Bot n Nothing (Two i1 i2) targets)
            zipWithM_ propagate [t1, t2] xs

propagate :: Target -> Int -> State (Map Target Node) ()
propagate target value = do
  m <- get
  case M.lookup target m of
    Just (Bot _ (Just (ActiveBot bf)) _ _) -> bf value
    Nothing -> let (O k) = target
               in modify $ M.insert target (Output k value)

parse :: [String] -> State (Map Target Node) ()
parse ["bot",n0,_,_,_,o1,n1,_,_,_,o2,n2] =
    let botNum = BotId $ read n0
        targets = Targets (f o1 n1) (f o2 n2)
        bot = Bot botNum (Just $ ActiveBot $ botStage1 botNum targets) None targets
    in modify $ M.insert (B botNum) bot
    where f o n
              | o == "output" = O (OutId $ read n)
              | o == "bot"    = B (BotId $ read n)
parse ["value",v,_,_,_,n] = propagate (B (BotId $ read n)) (read v)

buildGraph :: String -> Map Target Node
buildGraph = flip execState M.empty . mapM parse . sort . map words . lines

part1 :: String -> Int
part1 = coerce . bId . head . filter ((== Two 17 61) . inputs) . M.elems . buildGraph

part2 :: String -> Int
part2 s = let graph = buildGraph s
          in product . map input $ map ((graph !) . O . OutId) [0..2]
