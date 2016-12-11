module Year2016.Day10
    ( part1
    , part2
    ) where

import Control.Monad.State
import Data.List (sort)
import Data.Map.Strict ((!), Map)
import qualified Data.Map.Strict as M

data BotInputs = None | One Int | Two Int Int deriving (Eq)
data Target = B Int | O Int deriving (Eq, Ord)
data Targets = Targets Target Target

data Node = Bot { num :: Int
                , botFunc :: Maybe ActiveBot
                , inputs :: BotInputs
                , targets :: Targets
                }
          | Output { num :: Int
                   , input :: Int
                   }

data ActiveBot = ActiveBot (Int -> State (Map Target Node) ())

botStage1 :: Int -> Targets -> Int -> State (Map Target Node) ()
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
    let botNum = read n0
        targets = Targets (f o1 n1) (f o2 n2)
        bot = Bot botNum (Just $ ActiveBot $ botStage1 botNum targets) None targets
    in modify $ M.insert (B botNum) bot
    where f o n
              | o == "output" = O (read n)
              | o == "bot"    = B (read n)
parse ["value",v,_,_,_,n] = propagate (B (read n)) (read v)

buildGraph :: String -> Map Target Node
buildGraph input = execState (mapM parse (sort . map words $ lines input)) M.empty

part1 :: String -> String
part1 = show . num . head . filter ((== Two 17 61) . inputs) . M.elems . buildGraph

part2 :: String -> String
part2 s = let graph = buildGraph s
          in show . product . map input $ map ((graph !) . O) [0..2]
