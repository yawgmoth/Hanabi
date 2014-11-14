module Main (main) where

import System.Random
import Data.Array.IO
import Control.Monad
import Debug.Trace
import Data.List
import qualified Data.Map.Strict as Map
 


data Color = RED | YELLOW | GREEN | BLUE | WHITE deriving (Show, Eq, Ord)
type Value = Int 

type Card = (Color, Value)

data Move a = Discard Int | Play Int | HintColor Int Color | HintValue Int Value | Undecided a

instance Monad Move where
    Undecided x >>= f = f x
    Discard n >>= f = Discard n
    Play n >>= f = Play n
    HintColor n c >>= f = HintColor n c
    HintValue n v >>= f = HintValue n v
    return = Undecided

thedeck :: [ Card ]
thedeck = [ (c,v) | c <- [ RED, YELLOW, GREEN, BLUE, WHITE ], v <- toenumeration quantities ]

quantities :: [Int]
quantities = [3, 2, 2, 2, 1]

maxhints = 8

toenumeration :: [Int] -> [Int]
toenumeration (x:xs) = toenumeration' (x:xs) 1

toenumeration' :: [Int] -> Int -> [Int]
toenumeration' (0:xs) value = toenumeration' xs (value + 1)
toenumeration' (x:xs) value = value : toenumeration' (x-1:xs) value
toenumeration' [] _ = []
 
-- | Randomly shuffle a list
--   /O(N)/
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs
    
data GameState = GState {hands :: [[Card]], board :: [Card], trash :: [Card], knowledge :: [[Map.Map Card Int]], hints :: Int, fuse :: Int, deck :: [Card], extraturns :: Int} deriving (Show, Eq)

data HintType = COLOR | VALUE deriving (Eq, Show)
    
playsecure :: GameState -> Int -> Move [Int]
playsecure gs@GState {knowledge=k, hands=h, board=b} pnr = if (length $ possibleplays b (k!!pnr)) > 0 then Play ((possibleplays b (k!!pnr))!!0) else Undecided []

canplay :: [Card] -> Map.Map Card Int -> Bool
canplay board m = and (map (isvalidplay board) $ Map.keys (Map.filter (> 0) m))

possibleplays' :: [Card] -> [Map.Map Card Int] -> Int -> [Int]
possibleplays' board knowledge nr | nr == length knowledge = []
                                  | otherwise = if (canplay board (knowledge!!nr)) then nr:(possibleplays' board knowledge (nr+1)) else possibleplays' board knowledge (nr+1)

possibleplays :: [Card] -> [Map.Map Card Int] -> [Int]
possibleplays board knowledge = possibleplays' board knowledge 0

publicknowledge :: [Card] -> [Map.Map Card Int] -> [Map.Map Card Int]
publicknowledge [] k = k
publicknowledge (c:cs) k = publicknowledge cs $ map (Map.insertWith (+) c (-1)) k

gone :: [Card] -> [Card]
gone [] = []
gone ((col,val):cs) | val == 0  = gone cs
                    | otherwise = [(col,v) | v <- [1..val]] ++ (gone cs)


hintvalue :: GameState -> ([Card], [Map.Map Card Int], Int, Int, HintType) -> Int
hintvalue GState {board=b, trash=t, hands=h} (hand, knowledge, nr, card, COLOR) = length $ possibleplays b (publicknowledge (t ++ (gone b) ++ (concat otherhands)) (makecolorknowledgeplayer hand knowledge col))
                                                                   where (col,val) = hand!!card
                                                                         otherhands = ys ++ (tail zs)
                                                                         (ys,zs) = splitAt nr h
hintvalue GState {board=b, trash=t, hands=h} (hand, knowledge, nr, card, VALUE) = length $ possibleplays b (publicknowledge (t ++ (gone b) ++ (concat otherhands)) (makevalueknowledgeplayer hand knowledge val))
                                                                   where (col,val) = hand!!card
                                                                         otherhands = ys ++ (tail zs)
                                                                         (ys,zs) = splitAt nr h


makehint :: GameState -> ([Card], [Map.Map Card Int], Int, Int, HintType) -> Move [Int]
makehint gs (hand, knowledge, nr, card, COLOR) | (hintvalue gs (hand, knowledge, nr, card, COLOR)) > 0 = HintColor nr col
                                               | otherwise = Undecided []
                                       where (col,val) = hand!!card
makehint gs (hand, knowledge, nr, card, VALUE) | hintvalue gs (hand, knowledge, nr, card, VALUE) > 0 = HintValue nr val
                                               | otherwise = Undecided []
                                       where (col,val) = hand!!card

snd5 :: (a,b,c,d,e) -> b
snd5 (a,b,c,d,e) = b

thrd5 :: (a,b,c,d,e) -> c
thrd5 (a,b,c,d,e) = c
                                       
cmphintvalues :: GameState -> ([Card], [Map.Map Card Int], Int, Int, HintType) -> ([Card], [Map.Map Card Int], Int, Int, HintType) -> Ordering
cmphintvalues gs a b = compare ((hintvalue gs a) - pva) ((hintvalue gs b) - pvb)
                        where
                             pva = length $ possibleplays (board gs) (publicknowledge ((trash gs) ++ (gone (board gs)) ++ (concat otherhandsa)) (snd5 a))
                             pvb = length $ possibleplays (board gs) (publicknowledge ((trash gs) ++ (gone (board gs)) ++ (concat otherhandsb)) (snd5 b))
                             otherhandsa = ysa ++ (tail zsa)
                             (ysa,zsa) = splitAt (thrd5 a) h
                             otherhandsb = ysb ++ (tail zsb)
                             (ysb,zsb) = splitAt (thrd5 b) h
                             h = hands gs
                                       
givehint :: GameState -> Int -> Move [Int]
givehint gs@GState {hands=h, knowledge=k, hints=hnt} pnr | hnt > 0 = makehint gs $ maximumBy (cmphintvalues gs) $ [(h!!nr,k!!nr,nr,card,typ) |  
                                                                                                                                nr <- [0..(length k)-1], 
                                                                                                                                nr /= pnr,
                                                                                                                                (length (h!!nr)) > 0,
                                                                                                                                card <- [0..(length (k!!nr))-1], 
                                                                                                                                typ <- [COLOR, VALUE]]
                                                         | otherwise = Undecided []

addifpositive :: Int -> Int -> Int
addifpositive a b | b > 0     = a + b
                  | otherwise = a
                                                         
entropy' :: Map.Map Card Int -> Int
entropy' m = Map.foldl (addifpositive) 0 m

entropy :: [Map.Map Card Int] -> Int
entropy [] = 0
entropy (m:ms) = entropy' m + entropy ms

entropyhints = 4

hintentropy :: GameState -> ([Card], [Map.Map Card Int], Int, Int, HintType) -> Int
hintentropy gs (h,k,pnr,cnr,typ) | typ == COLOR = entropy (publicknowledge (t ++ (gone b) ++ (concat otherhands)) (makecolorknowledgeplayer h k col))
                                 | otherwise    = entropy (publicknowledge (t ++ (gone b) ++ (concat otherhands)) (makevalueknowledgeplayer h k val))
                               where
                                   t = trash gs
                                   b = board gs
                                   otherhands = ys ++ (tail zs)
                                   (ys,zs) = splitAt pnr allh
                                   allh = hands gs
                                   (col,val) = h!!cnr

cmphintentropy :: GameState -> ([Card], [Map.Map Card Int], Int, Int, HintType) -> ([Card], [Map.Map Card Int], Int, Int, HintType) -> Ordering
cmphintentropy gs hinta@(ha,ka,pnra,cnra,typa) hintb@(hb,kb,pnrb,cnrb,typb) = compare (pva - pka) (pvb - pkb)
                                         where
                                           pka = hintentropy gs hinta
                                           pkb = hintentropy gs hintb
                                           b = board gs
                                           t = trash gs
                                           h = hands gs
                                           pva = entropy (publicknowledge (t ++ (gone b) ++ (concat otherhandsa)) ka)
                                           pvb = entropy (publicknowledge (t ++ (gone b) ++ (concat otherhandsb)) kb)
                                           otherhandsa = ysa ++ (tail zsa)
                                           (ysa,zsa) = splitAt pnra h
                                           otherhandsb = ysb ++ (tail zsb)
                                           (ysb,zsb) = splitAt pnrb h

makeentropyhint :: GameState -> ([Card], [Map.Map Card Int], Int, Int, HintType) -> Move [Int]
makeentropyhint gs h@(hand, knowledge, nr, card, COLOR) | (hintentropy gs h) > 0 = HintColor nr col
                                                        | otherwise = Undecided []
                                           where (col,val) = hand!!card
makeentropyhint gs h@(hand, knowledge, nr, card, VALUE) | (hintentropy gs h) > 0 = HintValue nr val
                                                        | otherwise = Undecided []
                                           where (col,val) = hand!!card

givehintminentropy :: GameState -> Int -> Move [Int]
givehintminentropy gs@GState {hands=h, knowledge=k, hints=hnt} pnr | hnt > entropyhints = makeentropyhint gs $ maximumBy (cmphintentropy gs) $ [(h!!nr,k!!nr,nr,card,typ) |  
                                                                                                                                nr <- [0..(length k)-1], 
                                                                                                                                nr /= pnr,
                                                                                                                                (length (h!!nr)) > 0,
                                                                                                                                card <- [0..(length (k!!nr))-1], 
                                                                                                                                typ <- [COLOR, VALUE]]
                                                         | otherwise = Undecided []
                                                     where otherhands = ys ++ (tail zs)
                                                           (ys,zs) = splitAt pnr h
    
play :: GameState -> Int -> Move [Int]
play gs player = do
             playsecure gs player
             givehint gs player
             givehintminentropy gs player
             Discard 0
             Play 0
             
iscomplete :: [Card] -> Bool
iscomplete [] = True
iscomplete (x:xs) = ((snd x) == 5) && (iscomplete xs)

isover :: GameState -> Bool
isover GState {deck=[], extraturns=n, hands=h} = (n == length h)
isover GState {fuse=0} = True
isover GState {board=b} = iscomplete b

extractPlayerInformation :: GameState -> Int -> GameState
extractPlayerInformation gs@GState {hands=h} nr = gs {hands = ys ++ [[]] ++ (tail zs)}
                                        where (ys, zs) = splitAt nr h

discardone :: [Card] -> Int -> [Card]
discardone hand n = ys ++ (tail zs) 
                       where (ys,zs) = splitAt n hand
                       
drawone :: GameState -> [Card] -> [Card]
drawone GState {deck=[]} hand = hand
drawone GState {deck=(x:xs)} hand = hand ++ [x]

makediscard :: GameState -> Int -> Int -> [[Card]]
makediscard gs@GState {hands=h, deck=d} n player = ys ++ [ drawone gs $ (discardone (head zs) n) ] ++ (tail zs)
                               where (ys,zs) = splitAt player h 
                               
makedraw :: GameState -> [Card]
makedraw GState {deck=[]} = []
makedraw GState {deck=(x:xs)} = xs

makediscardtrash :: GameState -> Int -> Int -> [Card]
makediscardtrash GState {hands=h, trash=t} n player = newcard:t
                                    where newcard = h!!player!!n
                                    
makeplaytrash :: GameState -> Int -> Int -> [Card]
makeplaytrash gs@GState {hands=h, trash=t, board=b} n player = if (isvalidplay b newcard) then t else newcard:t
                                    where newcard = h!!player!!n
                               
makeextraturn :: GameState -> Int
makeextraturn GState {deck = [], extraturns=n} = n+1
makeextraturn GState {extraturns=n} = n

isvalidplay :: [Card] -> Card -> Bool
isvalidplay [] _ = False
isvalidplay ((ccol,cval):cs) (ncol,nval) | ncol == ccol && nval == cval + 1 = True
                                         | otherwise = isvalidplay cs (ncol,nval)

addcard :: [Card] -> Card -> [Card]
addcard [] _ = []
addcard ((ccol,cval):cs) (ncol,nval) | ncol == ccol && nval == cval + 1 = (ccol,nval):cs
addcard (c:cs) card = c:(addcard cs card)

makefuse :: GameState -> Int -> Int -> Int
makefuse gs@GState {board=b, hands=h, fuse=f} n player = if (isvalidplay b newcard) then f else f-1
                              where newcard = h!!player!!n

makeplay :: GameState -> Int -> Int -> [Card]
makeplay gs@GState {board=b, hands=h} n player = if (isvalidplay b newcard) then (addcard b newcard) else b
                              where newcard = h!!player!!n
                              

onlycolor :: Color -> Card -> Int -> Int
onlycolor col (ccol,cval) cnt | ccol == col = cnt
                              | otherwise = 0
                          
removecolor :: Color -> Card -> Int -> Int
removecolor col (ccol,cval) cnt | ccol == col = 0
                                | otherwise = cnt
                              
makecolorknowledgeplayer :: [Card] -> [Map.Map Card Int] -> Color -> [Map.Map Card Int]
makecolorknowledgeplayer [] [] _ = []
makecolorknowledgeplayer (c:cs) (m:ms) col | (fst c) == col = (Map.mapWithKey (onlycolor col) m):(makecolorknowledgeplayer cs ms col)
                                           | otherwise = (Map.mapWithKey (removecolor col) m):(makecolorknowledgeplayer cs ms col)
-- makecolorknowledgeplayer [] (m:ms) _ = trace (show (m:ms)) $ makecolorknowledgeplayer [] (m:ms) BLUE
-- makecolorknowledgeplayer (c:cs) [] _ = trace (show (c:cs)) $ makecolorknowledgeplayer (c:cs) [] BLUE

makecolorknowledge :: GameState -> Int -> Color -> [[Map.Map Card Int]]
makecolorknowledge gs@GState {hands=h, knowledge=k} player col = ys ++ [(makecolorknowledgeplayer (h!!player) (head zs) col)] ++ (tail zs)
                                                where (ys,zs) = splitAt player k
                                                
                                                
onlyvalue :: Int -> Card -> Int -> Int
onlyvalue val (ccol,cval) cnt | cval == val = cnt
                              | otherwise = 0
                          
removevalue :: Int -> Card -> Int -> Int
removevalue val (ccol,cval) cnt | cval == val = 0
                                | otherwise = cnt
                              
makevalueknowledgeplayer :: [Card] -> [Map.Map Card Int] -> Int -> [Map.Map Card Int]
makevalueknowledgeplayer [] [] _ = []
makevalueknowledgeplayer (c:cs) (m:ms) val | (snd c) == val = (Map.mapWithKey (onlyvalue val) m):(makevalueknowledgeplayer cs ms val)
                                           | otherwise = (Map.mapWithKey (removevalue val) m):(makevalueknowledgeplayer cs ms val)

makevalueknowledge :: GameState -> Int -> Int -> [[Map.Map Card Int]]
makevalueknowledge gs@GState {hands=h, knowledge=k} player val = ys ++ [(makevalueknowledgeplayer (h!!player) (head zs) val)] ++ (tail zs)
                                                where (ys,zs) = splitAt player k
                                                

updateplayerknowledge :: [Card] -> [Map.Map Card Int] -> Int -> [Map.Map Card Int]
updateplayerknowledge [] ms c = ys ++ (tail zs)
                             where (ys, zs) = splitAt c ms
updateplayerknowledge (x:xs) ms c = ys ++ (tail zs) ++ [basicknowledge]
                             where (ys, zs) = splitAt c ms
                    

updateknowledge :: GameState -> Int -> Int -> [[Map.Map Card Int]]
updateknowledge gs@GState {knowledge=k, deck=d} c player = ys ++ [(updateplayerknowledge d (head zs) c)] ++ (tail zs)
                                            where (ys, zs) = splitAt player k

makehints :: GameState -> Int
makehints GState {hints=h} = h-1

makehintsdiscard :: GameState -> Int
makehintsdiscard GState {hints=h} | h < maxhints = h + 1
                                  | otherwise    = maxhints
                                  
makehintsplay :: GameState -> Int -> Int -> Int
makehintsplay gs@GState {board=b, hands=h, hints=hnt} n player = if (isvalidplay b (col,val)) && (val == 5) then (makehintsdiscard gs) else hnt
                                                             where (col,val) = h!!player!!n

makenewgamestate :: GameState -> Move a -> Int -> GameState
makenewgamestate gs (Discard n) player = trace ("Discard " ++ (show n) ++ " from player " ++ (show player) ++ ", which is: " ++ (show $ (hands gs)!!player!!n) ++ " " ++ (show $ (length $ (knowledge gs)!!player))) gs { hands = makediscard gs n player, deck = makedraw gs, extraturns = makeextraturn gs, trash=makediscardtrash gs n player, knowledge=updateknowledge gs n player, hints=makehintsdiscard gs }
makenewgamestate gs (Play n) player = trace ("Play " ++ (show n) ++ " from player " ++ (show player) ++ ", which is: " ++ (show $ (hands gs)!!player!!n) ++ " board now: " ++ (show (makeplay gs n player)) ++ " " ++ (show $ (length $ (knowledge gs)!!player))) gs { hands = makediscard gs n player, deck = makedraw gs, extraturns = makeextraturn gs, board = makeplay gs n player, trash = makeplaytrash gs n player, fuse=makefuse gs n player, knowledge=updateknowledge gs n player, hints=makehintsplay gs n player  }
makenewgamestate gs (HintColor n c) player = trace ("Hint " ++ (show n) ++ ", " ++ (show c) ++ " from player " ++ (show player) ++ ", which is: " ++ (show $ (hands gs)!!n) ++ " " ++ (show $ (length $ (knowledge gs)!!player))) gs { hints=makehints gs, knowledge=makecolorknowledge gs n c}
makenewgamestate gs (HintValue n v) player = trace ("Hint " ++ (show n) ++ ", " ++ (show v) ++ " from player " ++ (show player) ++ ", which is: " ++ (show $ (hands gs)!!n) ++ " " ++ (show $ (length $ (knowledge gs)!!player))) gs { hints=makehints gs, knowledge=makevalueknowledge gs n v}
makenewgamestate gs _ _ = gs

score :: GameState -> Int
score GState {board=b} = sum (map snd b)

turn :: GameState -> Int -> Int
turn gs@GState {hands=h} player = if (isover gs) then (score gs) else turn nextstate nextplayer
                 where
                    nextplayer = ((player + 1) `mod` (length h))
                    nextstate = makenewgamestate gs (play (extractPlayerInformation gs player) player) player
                    
basicknowledge :: Map.Map Card Int
basicknowledge = Map.fromList $ zip [ (col,c) :: Card | col <- [ RED, YELLOW, GREEN, BLUE, WHITE ], c <- [1..5]] $ cycle quantities

main = do
       sdeck <- shuffle thedeck
       let score = turn GState {deck=(drop 15 sdeck), hands=[(take 5 sdeck), (take 5 $ drop 5 sdeck), (take 5 $ drop 10 sdeck)], trash=[], extraturns=0, fuse=3, knowledge=[[basicknowledge | c1 <- [1..5]] | p <- [1..3]], hints=maxhints, board=[ (c,0) | c <- [ RED, YELLOW, GREEN, BLUE, WHITE ] ]} 0
       putStrLn $ show score
       