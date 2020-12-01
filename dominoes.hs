import Data.List
import System.Random
import Debug.Trace

type Domino = (Int,Int)

type Hand = [Domino]

type Board = [Domino]

data End = L | R

canPlay :: Domino -> End -> Board -> Bool
-- canPlay is True when the board is empty
canPlay domino end [] = True
canPlay (l,r) L ((pips,_):_)
    | l == pips = True
    | r == pips = True
    | otherwise = False
canPlay (l,r) R board
    | l == pips = True
    | r == pips = True
    | otherwise = False
    where
        (_,pips) = last board

blocked :: Hand -> Board -> Bool
-- hand is blocked if the player has no dominoes in a hand or if they can't play on L/r end and 
-- rest of dominoes are blocked
blocked [] board = True
blocked (d:ds) board = not (canPlay d L board) && not (canPlay d R board) && blocked ds board

played :: Domino -> Board -> Bool
played (_,_) [] = False
played (l,r) board = elem (l,r) board && elem (r,l) board

possPlays :: Hand -> Board -> ([Domino],[Domino])
possPlays hand board
    = possPlays' hand board ([],[])
      where
      possPlays' [] board possibilities = possibilities
      possPlays' (d:ds) board (leftPoss,rightPoss)
        = possPlays' ds board (ls, rs)
          where
          ls | canPlay d L board = d:leftPoss
             | otherwise = leftPoss
          rs | canPlay d R board = d:rightPoss
             | otherwise = rightPoss

playDom :: Domino -> Board -> End -> Maybe Board
playDom domino [] end = Just [domino]
playDom (l,r) board@((pips,_):_) L 
    | l == pips = Just ((r,l):board)
    | r == pips = Just ((l,r):board)
    | otherwise = Nothing
playDom (l,r) board R
    | l == pips = Just ((r,l):board)
    | r == pips = Just ((l,r):board)
    | otherwise = Nothing
        where
            (_,pips) = last board

-- possible score combinations taken from solution
scoreConverter :: Int -> Int
scoreConverter n 
    | n == 3    = 1 -- 1 "3"
    | n == 5    = 1 -- 1 "5"
    | n == 6    = 2 -- 2 "3"s
    | n == 9    = 3 -- 2 "3"s
    | n == 10   = 2 -- 2 "5"s
    | n == 12   = 4 -- 4 "3"s
    | n == 15   = 8 -- 5 "3"s + 3 "5"s
    | n == 18   = 6 -- 6 "3"s
    | n == 20   = 4 -- 4 "5"s
    | otherwise = 0 -- not a multiple of 3 or 5

scoreBoard :: Board -> Int
scoreBoard board@((l,r):_)
    | l == r = scoreConverter (l+r+rightendPips)
    | otherwise = scoreConverter (l+rightendPips)
    where
        (_,rightendPips) = last board

scoreN :: Board -> Int ->[(Domino, End)]
scoreN board n = scoreN' board domSet []
                 where
                 scoreN' board [] options = options
                 scoreN' board (domino:rest) options
                    | played domino board = scoreN' board rest options -- skip this domino if already played
                    | otherwise = scoreN' board rest newOptions
                      where
                      leftBoard = playDom domino board L    -- try playing it on the left end
                      rightBoard = playDom domino board R   -- try playing it on the right end
                      -- now for each try, see 1) if it was legal and 2) if it achieved the desired score
                      goodLeft = leftBoard /= Nothing && scoreB leftBoard == n 
                      goodRight = rightBoard /= Nothing && scoreB rightBoard == n
                      -- scoreB is only going to be used if it is Just something, so grab the "something"
                      scoreB (Just board) = scoreBoard board
                      newOptions
                        | goodLeft && goodRight = (domino,L):(domino,R):options -- play either end for this score
                        | goodLeft = (domino,L):options -- left end only for this score
                        | goodRight = (domino,R):options -- right end only for this score
                        | otherwise = options   -- can't achieve this score with this domino

type DomsPlayer = Hand -> Board -> (Domino, End)

simplePlayer :: DomsPlayer
simplePlayer (domino:rest) board
    | playDom domino board L /= Nothing = (domino,L)
    | playDom domino board R /= Nothing = (domino,R) 
    | otherwise = simplePlayer rest board

shuffleDoms :: StdGen -> [Domino]
cmp (x1,y1) (x2,y2) = compare y1 y2
shuffleDoms gen = [leftPips | (leftPips, n) <- sortBy cmp (zip domSet (randoms gen :: [Int]))]

domSet = [(l,r) | l <- [0..6], r <- [0..6]]