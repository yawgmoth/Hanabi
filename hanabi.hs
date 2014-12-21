module Main (main) where

import System.Random
import Data.Array.IO
import Data.Ratio
import Control.Monad
import Debug.Trace
import Data.List
import qualified Data.Map.Strict as Map
 


data Color = RED | YELLOW | GREEN | BLUE | WHITE deriving (Show, Eq, Ord)
type Value = Int 

type Card = (Color, Value)

data Move a = Discard Int | DiscardU Int | Play Int | HintColor Int Color | HintValue Int Value | Undecided a deriving (Show)

instance Monad Move where
    Undecided x >>= f = f x
    Discard n >>= f = Discard n
    DiscardU n >>= f = DiscardU n
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
playsecure gs@GState {knowledge=k, hands=h, board=b} pnr = if (length $ possibleplays b pk) > 0 then Play ((possibleplays b pk)!!0) else Undecided []
                                                    where
                                                       pk = publicknowledge (t ++ g ++ (concat h)) (k!!pnr)
                                                       t = trash gs
                                                       g = gone b

playcnt :: [Card] -> Map.Map Card Int -> Int
playcnt board m = sum $ Map.elems (Map.filterWithKey (\k v -> (v > 0) && (isvalidplay board k)) m)

playperc :: [Card] -> Map.Map Card Int -> Ratio Int
playperc  board m = (playcnt board m) % (sum (Map.elems (Map.filter (> 0) m)))

probableplays' :: Ratio Int -> [Card] -> [Map.Map Card Int] -> Int -> [(Int, Ratio Int)]
probableplays' limit board knowledge nr | nr == length knowledge = []
                                  | otherwise = if (perc > limit) then (nr,perc):(probableplays' limit board knowledge (nr+1)) else probableplays' limit board knowledge (nr+1)
                                           where 
                                               perc = playperc board (knowledge!!nr)

probableplays :: Ratio Int -> [Card] -> [Map.Map Card Int] -> [(Int, Ratio Int)]
probableplays limit board knowledge = probableplays' limit board knowledge 0

cmpsnd :: Ord b => (a,b) -> (a,b) -> Ordering
cmpsnd (_,x) (_,y) = compare x y

playprobable :: Ratio Int -> GameState -> Int -> Move [Int]
playprobable limit gs@GState {knowledge=k, hands=h, board=b} pnr = if (length $ probableplays limit b pk) > 0 then Play $ fst $ maximumBy (cmpsnd) $ probableplays limit b pk else Undecided []
                                                         where
                                                           pk = publicknowledge (t ++ g ++ (concat h)) (k!!pnr)
                                                           t = trash gs
                                                           g = gone b

showprobs gs@GState {knowledge=k, hands=h, board=b} pnr = trace ("Board: " ++ (show b) ++ "\nTrash: " ++ (show t) ++ "\nknowledge:\n" ++ pks ++ "\nprobs:" ++ (show probs) ++ "\nplaycnt: " ++ (show plc) ++ "\n\n") Undecided []
                                                         where
                                                           pks = concat (map (\(i,k) -> "card " ++ (show i) ++ " " ++ (show k) ++ "\n") $ zip [0..] pk)
                                                           pk = map (Map.filter (>0)) $ publicknowledge (t ++ g ++ (concat h)) (k!!pnr)
                                                           t = trash gs
                                                           g = gone b
                                                           probs = map (playperc b) pk
                                                           plc = map (playcnt b) pk
                                                           
minfuse :: Int -> GameState -> Move [Int] -> Move [Int]
minfuse minf gs@GState {fuse=f} m | f >= minf = m
                                  | otherwise = Undecided []

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

entropyhints = 6
usefulhints = 2

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
makeentropyhint gs h@(hand, knowledge, nr, card, COLOR) | prev - (hintentropy gs h) > 0 = HintColor nr col
                                                        | otherwise = Undecided []
                                           where (col,val) = hand!!card
                                                 prev = entropy (publicknowledge (t ++ (gone b) ++ (concat otherhands)) knowledge)
                                                 t = trash gs
                                                 hs = hands gs
                                                 b = board gs
                                                 otherhands = ys ++ (tail zs)
                                                 (ys,zs) = splitAt nr hs
makeentropyhint gs h@(hand, knowledge, nr, card, VALUE) | prev - (hintentropy gs h) > 0 = HintValue nr val
                                                        | otherwise = Undecided []
                                           where (col,val) = hand!!card
                                                 prev = entropy (publicknowledge (t ++ (gone b) ++ (concat otherhands)) knowledge)
                                                 t = trash gs
                                                 hs = hands gs
                                                 b = board gs
                                                 otherhands = ys ++ (tail zs)
                                                 (ys,zs) = splitAt nr hs
                                           

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

usefulcard :: [Card] -> Map.Map Card Int -> Int
usefulcard b m = length [c | c <- (Map.keys $ Map.filter (> 0) m), not $ isvalidplay b c]
                                                           
useful :: [Card] -> [Map.Map Card Int] -> Int
useful _ [] = 0
useful b (m:ms) = usefulcard b m + (useful b ms)
                                                           
hintuse :: GameState -> ([Card], [Map.Map Card Int], Int, Int, HintType) -> Int
hintuse gs (h,k,pnr,cnr,typ) | typ == COLOR = useful (board gs) (publicknowledge (t ++ (gone b) ++ (concat otherhands)) (makecolorknowledgeplayer h k col))
                             | otherwise    = useful (board gs) (publicknowledge (t ++ (gone b) ++ (concat otherhands)) (makevalueknowledgeplayer h k val))
                               where
                                   t = trash gs
                                   b = board gs
                                   otherhands = ys ++ (tail zs)
                                   (ys,zs) = splitAt pnr allh
                                   allh = hands gs
                                   (col,val) = h!!cnr
                                   
cmphintuseful :: GameState -> ([Card], [Map.Map Card Int], Int, Int, HintType) -> ([Card], [Map.Map Card Int], Int, Int, HintType) -> Ordering
cmphintuseful gs hinta@(ha,ka,pnra,cnra,typa) hintb@(hb,kb,pnrb,cnrb,typb) = compare (pva - pka) (pvb - pkb)
                                         where
                                           pka = hintuse gs hinta
                                           pkb = hintuse gs hintb
                                           b = board gs
                                           t = trash gs
                                           h = hands gs
                                           pva = useful (board gs) (publicknowledge (t ++ (gone b) ++ (concat otherhandsa)) ka)
                                           pvb = useful (board gs) (publicknowledge (t ++ (gone b) ++ (concat otherhandsb)) kb)
                                           otherhandsa = ysa ++ (tail zsa)
                                           (ysa,zsa) = splitAt pnra h
                                           otherhandsb = ysb ++ (tail zsb)
                                           (ysb,zsb) = splitAt pnrb h
                                                           
makeusefulhint :: GameState -> ([Card], [Map.Map Card Int], Int, Int, HintType) -> Move [Int]
makeusefulhint gs h@(hand, knowledge, nr, card, COLOR) | (hintuse gs h) > 0 = HintColor nr col
                                                       | otherwise = Undecided []
                                           where (col,val) = hand!!card
makeusefulhint gs h@(hand, knowledge, nr, card, VALUE) | (hintuse gs h) > 0 = HintValue nr val
                                                       | otherwise = Undecided []
                                           where (col,val) = hand!!card
                                                           
givehintuseful :: GameState -> Int -> Move [Int]
givehintuseful gs@GState {hands=h, knowledge=k, hints=hnt} pnr | hnt > usefulhints = makeusefulhint gs $ maximumBy (cmphintuseful gs) $ [(h!!nr,k!!nr,nr,card,typ) |  
                                                                                                                                nr <- [0..(length k)-1], 
                                                                                                                                nr /= pnr,
                                                                                                                                (length (h!!nr)) > 0,
                                                                                                                                card <- [0..(length (k!!nr))-1], 
                                                                                                                                typ <- [COLOR, VALUE]]
                                                         | otherwise = Undecided []
                                                     where otherhands = ys ++ (tail zs)
                                                           (ys,zs) = splitAt pnr h

useless :: Map.Map Card Int -> [Card] -> Bool
useless m b = and $ map ((flip elem) b) $ Map.keys $ Map.filter (> 0) m

makeuselessdiscard :: [Map.Map Card Int] -> [Card] -> Int -> Move [Int]
makeuselessdiscard [] _ _ = Undecided []
makeuselessdiscard (m:ms) b nr | (useless m b) = Discard nr
                               | otherwise     = makeuselessdiscard ms b (nr+1)

discarduseless :: GameState -> Int -> Move [Int]
discarduseless gs pnr | (hints gs) < maxhints = makeuselessdiscard k g 0
                      | otherwise             = Undecided []
                         where 
                             k = (knowledge gs)!!pnr
                             g = gone $ board gs

safecard :: [Card] -> [Card] -> Card -> Bool
safecard b t c = (c `elem` b || not (c `elem` t)) && (val < 5)
           where
               (col,val) = c
                             
safe :: Map.Map Card Int -> [Card] -> [Card] -> Bool
safe m b t = and $ map (safecard b t) $ Map.keys $ Map.filter (> 0) m
                             
makesafediscard :: [Map.Map Card Int] -> [Card] -> [Card] -> Int -> Move [Int]
makesafediscard [] _ _ _                        = Undecided []
makesafediscard (m:ms) b t nr | (safe m b t)    = Discard nr
                              | otherwise       = makesafediscard ms b t (nr+1)
                             
discardsafe :: GameState -> Int -> Move [Int]
discardsafe gs pnr | (hints gs) < maxhints = makesafediscard (publicknowledge (g ++ t ++ (concat $ hands gs)) k) g t 0
                   | otherwise             = Undecided []
                         where 
                             k = (knowledge gs)!!pnr
                             g = gone $ board gs
                             t = trash gs
                             
necessarycount :: GameState -> (Map.Map Card Int,Int) -> Int
necessarycount gs (k,_) = sum [Map.findWithDefault 0 c k | c <- Map.keys $ Map.filter (> 0) k, not (safecard b t c)]
                  where
                     b = gone $ board gs
                     t = trash gs
                             
necessary :: GameState -> (Map.Map Card Int,Int) -> (Map.Map Card Int,Int) -> Ordering
necessary gs k1 k2 = (necessarycount gs k1) `compare` (necessarycount gs k2)
             
                 
makeunsafediscard :: (Map.Map Card Int,Int) -> Move [Int]
makeunsafediscard (_,nr) = DiscardU nr

discardunsafe :: GameState -> Int -> Move [Int]
discardunsafe gs pnr | (hints gs) < maxhints || (fuse gs) < 2 = makeunsafediscard $ minimumBy (necessary gs) $ zip (publicknowledge (g ++ t ++ (concat $ hands gs)) k) [0..]
                     | otherwise                              = Undecided []
                         where 
                             k = (knowledge gs)!!pnr
                             g = gone $ board gs
                             t = trash gs


play :: GameState -> Int -> Move [Int]
play gs player = do
             playsecure gs player
             -- showprobs gs player
             minfuse 3 gs $ playprobable (6 % 10) gs player
             minfuse 2 gs $ playprobable (8 % 10) gs player
             minfuse 1 gs $ playprobable (9 % 10) gs player
             givehint gs player
             givehintuseful gs player
             givehintminentropy gs player
             discarduseless gs player
             discardsafe gs player
             discardunsafe gs player
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

makenewgamestate :: (String -> GameState -> GameState) -> GameState -> Move a -> Int -> GameState
makenewgamestate t gs (Discard n) player = t ("Discard " ++ (show n) ++ " from player " ++ (show player) ++ ", which is: " ++ (show $ (hands gs)!!player!!n) ++ " " ++ (show $ (length $ (knowledge gs)!!player))) gs { hands = makediscard gs n player, deck = makedraw gs, extraturns = makeextraturn gs, trash=makediscardtrash gs n player, knowledge=updateknowledge gs n player, hints=makehintsdiscard gs }
makenewgamestate t gs (DiscardU n) player = t ("UNSAFE Discard " ++ (show n) ++ " from player " ++ (show player) ++ ", which is: " ++ (show $ (hands gs)!!player!!n) ++ " " ++ (show $ (length $ (knowledge gs)!!player))) gs { hands = makediscard gs n player, deck = makedraw gs, extraturns = makeextraturn gs, trash=makediscardtrash gs n player, knowledge=updateknowledge gs n player, hints=makehintsdiscard gs }
makenewgamestate t gs (Play n) player = t ("Play " ++ (show n) ++ " from player " ++ (show player) ++ ", which is: " ++ (show $ (hands gs)!!player!!n) ++ " board now: " ++ (show (makeplay gs n player)) ++ " " ++ (show $ (length $ (knowledge gs)!!player))) gs { hands = makediscard gs n player, deck = makedraw gs, extraturns = makeextraturn gs, board = makeplay gs n player, trash = makeplaytrash gs n player, fuse=makefuse gs n player, knowledge=updateknowledge gs n player, hints=makehintsplay gs n player  }
makenewgamestate t gs (HintColor n c) player = t ("Hint " ++ (show n) ++ ", " ++ (show c) ++ " from player " ++ (show player) ++ ", which is: " ++ (show $ (hands gs)!!n) ++ " " ++ (show $ (length $ (knowledge gs)!!player))) gs { hints=makehints gs, knowledge=makecolorknowledge gs n c}
makenewgamestate t gs (HintValue n v) player = t ("Hint " ++ (show n) ++ ", " ++ (show v) ++ " from player " ++ (show player) ++ ", which is: " ++ (show $ (hands gs)!!n) ++ " " ++ (show $ (length $ (knowledge gs)!!player))) gs { hints=makehints gs, knowledge=makevalueknowledge gs n v}
makenewgamestate t gs _ _ = gs

score :: GameState -> Int
score GState {board=b} = sum (map snd b)

turn :: (String -> GameState -> GameState) -> GameState -> Int -> Int
turn t gs@GState {hands=h} player = niltrace ("Board: " ++ (show $ board gs) ++ ", hints: " ++ (show $ hints gs)) (if (isover gs) then (score gs) else turn t nextstate nextplayer)
                 where
                    nextplayer = ((player + 1) `mod` (length h))
                    nextstate = makenewgamestate t gs (play (extractPlayerInformation gs player) player) player
                    
basicknowledge :: Map.Map Card Int
basicknowledge = Map.fromList $ zip [ (col,c) :: Card | col <- [ RED, YELLOW, GREEN, BLUE, WHITE ], c <- [1..5]] $ cycle quantities

niltrace s gs = gs

gsfromhands :: [[Card]] -> GameState
gsfromhands h = GState { deck = [], hands = h, trash=[], extraturns=0, fuse=3, knowledge=[[basicknowledge | c1 <- [1..5]] | p <- [1..3]], hints=maxhints, board=[ (c,0) | c <- [ RED, YELLOW, GREEN, BLUE, WHITE ] ]}

onegame t = do 
       sdeck <- shuffle thedeck
       let score = turn t GState {deck=(drop 15 sdeck), hands=[(take 5 sdeck), (take 5 $ drop 5 sdeck), (take 5 $ drop 10 sdeck)], trash=[], extraturns=0, fuse=3, knowledge=[[basicknowledge | c1 <- [1..5]] | p <- [1..3]], hints=maxhints, board=[ (c,0) | c <- [ RED, YELLOW, GREEN, BLUE, WHITE ] ]} 0
       let s1 = if score > 19 then turn trace GState {deck=(drop 15 sdeck), hands=[(take 5 sdeck), (take 5 $ drop 5 sdeck), (take 5 $ drop 10 sdeck)], trash=[], extraturns=0, fuse=3, knowledge=[[basicknowledge | c1 <- [1..5]] | p <- [1..3]], hints=maxhints, board=[ (c,0) | c <- [ RED, YELLOW, GREEN, BLUE, WHITE ] ]} 0 else 0
       return score
       
ngames 0 t = return 0
ngames n t = do score <- onegame niltrace
                score1 <- ngames (n-1) t
                return (score + score1)
   

main = do
        score100 <- ngames 100 niltrace
        putStrLn $ show $ score100 % 100

{-       sdeck <- shuffle thedeck
       let score = turn GState {deck=(drop 15 sdeck), hands=[(take 5 sdeck), (take 5 $ drop 5 sdeck), (take 5 $ drop 10 sdeck)], trash=[], extraturns=0, fuse=3, knowledge=[[basicknowledge | c1 <- [1..5]] | p <- [1..3]], hints=maxhints, board=[ (c,0) | c <- [ RED, YELLOW, GREEN, BLUE, WHITE ] ]} 0
       putStrLn $ show score -}
       