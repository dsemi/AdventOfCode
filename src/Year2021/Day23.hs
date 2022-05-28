module Year2021.Day23
    ( part1
    , part2
    ) where

import Control.Lens
import Data.Bits
import Data.Bool
import Data.Char
import Data.List (foldl')
import qualified Data.PQueue.Min as Q
import Data.Vector ((!), Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Data.Word

roomXor :: Word32
roomXor = 0xffaa5500

cost :: [Word16]
cost = [1, 10, 100, 1000]

safeSkips :: [Word32]
safeSkips = [ 0, 0, 0, 0, 0, 0, 0, 0
            , 0, 0, 0x3, 0x7, 0x7, 0x7, 0x7, 0
            , 0, 0, 0x3, 0x707, 0xf0f, 0xf0f, 0xf0f, 0
            , 0, 0, 0x3, 0x707, 0xf0f0f, 0x1f1f1f, 0x1f1f1f, 0
            ]

readInput :: String -> Word32
readInput = go 0 0
    where toGlyph c = fromIntegral $ ord c - ord 'A'
          go i room str
              | i == 4 = room `xor` (roomXor .&. 0x0f0f0f0f)
              | otherwise = go (i+1) room' $ drop 2 str
              where room' = room .|. (toGlyph (str !! 31) `shiftL` (8 * i))
                            .|. (toGlyph (str !! 45) `shiftL` (8 * i + 2))

insertPart2 :: Word32 -> Word32
insertPart2 room = ((room .&. 0x03030303) .|. (shiftL room 4 .&. 0xc0c0c0c0)) `xor` 0x1c2c0c3c

baseCost :: Word32 -> (Int, Int)
baseCost rooom =
    let (_, base, secondRow) = foldl' go (fromIntegral rooom `xor` fromIntegral roomXor, 0, 0) [0..3]
    in (base + secondRow * 2 + 3333, base + secondRow * 4 + 29115)
    where moveCost dist = 2 * max 1 (abs dist)
          go (room, base, secondRow) i =
              let glyph0 = room .&. 3
                  glyph1 = shiftR room 2 .&. 3
                  cost0 = fromIntegral $ cost !! fromIntegral glyph0
                  cost1 = fromIntegral $ cost !! fromIntegral glyph1
              in ( shiftR room 8
                 , base + (cost0 * (moveCost (i - glyph0) + 1)) + (cost1 * moveCost (i - glyph1))
                 , secondRow + cost1)

newtype Room = Room Word32 deriving (Show)

emptyRoom :: Room -> Int -> Bool
emptyRoom (Room room) r = shiftR room (8 * r) .&. 0xff  == 0

getRoom :: Room -> Int -> Word32
getRoom (Room room) r = fromIntegral r `xor` fromIntegral (shiftR room (8 * r) .&. 3)

popRoom :: Room -> Int -> Room
popRoom (Room room) r = let mask1 = shiftL 0xff (8 * r)
                            mask2 = shiftL 0x3f (8 * r)
                        in Room $ (shiftR room 2 .&. mask2) .|. (room .&. complement mask1)

newtype Hall = Hall Word32 deriving (Show)

emptyHall :: Hall -> Int -> Bool
emptyHall (Hall hall) h = hall .&. shiftL 4 (4 * h) == 0

clearHall :: Hall -> Int -> Hall
clearHall (Hall hall) h = Hall $ hall .&. complement (shiftL 0xf (4 * h))

setHall :: Hall -> Int -> Int -> Hall
setHall (Hall hall) h g = Hall $ hall .|. shiftL (4 .|. fromIntegral g) (4 * h)

getHall :: Hall -> Int -> Int
getHall (Hall hall) h = fromIntegral $ shiftR hall (4 * h) .&. 3

maskHall :: Hall -> Word32
maskHall (Hall hall) = hall .&. 0x4444444

data GameState = GameState Room Hall deriving (Show)

solved :: GameState -> Bool
solved (GameState (Room room) (Hall hall)) = room .|. hall == 0

roomL :: Int -> Int
roomL r = r + 1

roomR :: Int -> Int
roomR r = r + 2

newState :: Word64 -> GameState
newState hsh = GameState (Room $ fromIntegral hsh) (Hall $ fromIntegral $ shiftR hsh 32)

hashState :: GameState -> Word64
hashState (GameState (Room room) (Hall hall)) = shiftL (fromIntegral hall) 32 .|. fromIntegral room

obstructed :: GameState -> Int -> Int -> Bool
obstructed (GameState _ (Hall hall)) r h =
    let (lo, hi) = if h <= roomL r
                   then (h + 1, roomL r)
                   else (roomR r, h - 1)
        mask = shiftL 16 (4 * hi) - shiftL 1 (4 * lo)
    in hall .&. mask /= 0

bits :: (FiniteBits b, Num b) => b -> [Int]
bits = map countTrailingZeros . takeWhile (/=0) . iterate (\m -> m .&. (m - 1))

forceOne :: GameState -> (Bool, GameState)
forceOne st@(GameState room hall) = let (b, st') = checkHallToRoom $ bits $ maskHall hall
                                    in if b then (b, st')
                                       else checkRoomToRoom [0..3]
    where checkHallToRoom [] = (False, st)
          checkHallToRoom (b:bs)
              | emptyRoom room r && not (obstructed st r h) = (True, GameState room (clearHall hall h))
              | otherwise = checkHallToRoom bs
              where h = b `div` 4
                    r = getHall hall h
          checkRoomToRoom [] = (False, st)
          checkRoomToRoom (r:rs)
              | emptyRoom room r || g == r || not (emptyRoom room g) = checkRoomToRoom rs
              | not (obstructed st r $ if r < g then roomR g else roomL g) = (True, GameState (popRoom room r) hall)
              | otherwise = checkRoomToRoom rs
              where g = fromIntegral $ getRoom room r

deadlocked :: GameState -> Bool
deadlocked (GameState _ (Hall hall)) =
    let h43 = hall .&. 0x0077000
        h42 = hall .&. 0x0070700
        h32 = hall .&. 0x0007700
    in h43 == 0x0047000 || h43 == 0x0057000 ||
       h42 == 0x0040700 ||
       h32 == 0x0004600 || h32 == 0x0004700

crowded :: GameState -> Bool
crowded (GameState (Room room) (Hall hall)) =
    (\(_, _, _, s) -> not s) $ foldl' go (0, 0, shiftR hall 2 .|. 0x10000000, False) [0..7]
    where go (h0, h1, h, satisfied) i
              | h .&. 1 /= 0 && h0 < i =
                  let satisfied' = foldl' go2 satisfied [r0..r1]
                  in (i+1, h1, shiftR h 4, satisfied')
              | h .&. 1 /= 0 = (i+1, h1, shiftR h 4, satisfied)
              | otherwise = (h0, h1, shiftR h 4, satisfied)
              where r0 = max 0 (h0 - 2)
                    r1 = min 3 (i - 2)
                    space = i - h0
                    mask = shiftL 3 (2 * space)
                    go2 s r = let rr = shiftR room (8 * r) .&. 0xff
                              in if rr .&. mask == 0 then True else s

neighbors :: GameState -> Int -> [(Word16, GameState, Word32)]
neighbors st@(GameState room hall) skip = go [0..3]
    where skipRooms :: Int
          skipRooms = foldl' (\sr i -> if emptyHall hall (i+2)
                                       then sr
                                       else let mask = shiftL 14 i
                                            in sr .|. (if i < getHall hall (i+2)
                                                       then complement mask else mask)) 0 [0..2]
          go [] = []
          go (r:rs)
              | skipRooms .&. shiftL 1 r /= 0 || emptyRoom room r = go rs
              | otherwise = go2 [0..7] ++ go rs
              where g = fromIntegral $ getRoom room r
                    (lo, hi) = case compare r g of
                                 LT -> (roomR r, roomL g)
                                 GT -> (roomR g, roomL r)
                                 EQ -> (roomL r, roomR r)
                    go2 [] = []
                    go2 (h:hs)
                        | r /= g && h >= lo && h <= hi = go2 hs
                        | shiftR skip skipIdx .&. 1 /= 0 = go2 hs
                        | not (emptyHall hall h) || obstructed st r h = go2 hs
                        | deadlocked st2 = go2 hs
                        | otherwise = let (st3, skips) = go3 st2 $ safeSkips !! skipIdx
                                      in if crowded st3 then go2 hs
                                         else ((fromIntegral $ cost'' * fromIntegral (cost !! g), st3, skips) :)
                                                  $ go2 hs
                        where skipIdx = 8 * r + h
                              cost' = 2 * (if h < lo then lo - h else if hi < h then h - hi else 0)
                              a = bool 0 1 $ (bool (0::Int) 1 (cost' == 0) .|. bool 0 1 (r == g)) == 0
                              cost'' = 2 * (cost' - (a + (bool 0 1 (h == 0) .|. bool 0 1 (h == 6))))
                              room' = popRoom room r
                              hall' = setHall hall h g
                              st2 = GameState room' hall'
                              go3 st skips
                                  | b = go3 next 0
                                  | otherwise = (next, skips)
                                  where (b, next) = forceOne st

newtype Hash = Hash (Vector (Word64, (Word16, Word32)))

size :: Int
size = 14983

findHash :: Hash -> Word64 -> Int
findHash (Hash table) key = go $ fromIntegral key `mod` fromIntegral size
    where go idx
              | first /= 0 && first /= complement key = go $ (idx+1) `mod` size
              | otherwise = idx
              where first = fst (table ! idx)

insertHash :: Hash -> Word64 -> (Word16, Word32) -> Hash
insertHash hsh@(Hash table) key value =
    let idx = findHash hsh key
    in Hash $ V.modify (\v -> MV.write v idx (complement key, value)) table

setHash :: Hash -> Int -> (Word16, Word32) -> Hash
setHash (Hash table) idx value =
    Hash $ V.modify (\v -> MV.read v idx >>= MV.write v idx . set _2 value) table

getHash :: Hash -> Int -> (Word16, Word32)
getHash (Hash table) idx = snd $ table ! idx

existsHash :: Hash -> Int -> Bool
existsHash (Hash table) idx = fst (table ! idx) /= 0

solve :: GameState -> Int
solve start = go (insertHash (Hash (V.replicate size (0, (0, 0)))) (hashState start) (0, 0))
              $ Q.singleton (0, hashState start)
    where go cst q
            | Q.null q = fromIntegral $ fst $ getHash cst (findHash cst 0)
            | queueCost /= curCost = go cst q'
            | solved cur = go cst Q.empty
            | otherwise = go cst' q''
            where ((queueCost, curHash), q') = Q.deleteFindMin q
                  (curCost, curSkips) = getHash cst (findHash cst curHash)
                  cur = newState curHash
                  (cst', q'') = foldl' go2 (cst, q') $ neighbors cur $ fromIntegral curSkips
                  go2 (cst, q) (delta, state, skips) =
                      let hash = hashState state
                          newCost = curCost + delta
                          newIdx = findHash cst hash
                          (prevCost, prevSkips) = getHash cst newIdx
                      in if not (existsHash cst newIdx)
                         then (insertHash cst hash (newCost, skips), Q.insert (newCost, hash) q)
                         else if newCost == prevCost
                              then (setHash cst newIdx (prevCost, prevSkips .&. skips), q)
                              else if newCost < prevCost
                                   then (setHash cst newIdx (newCost, skips), Q.insert (newCost, hash) q)
                                   else (cst, q)

part1 :: String -> Int
part1 input = let room = readInput input
                  base = fst $ baseCost room
              in base + solve (newState (fromIntegral room))

part2 :: String -> Int
part2 input = let room = readInput input
                  base = snd $ baseCost room
              in base + solve (newState (fromIntegral (insertPart2 room)))
