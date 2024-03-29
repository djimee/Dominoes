-- pre-assignment task

module Dominoes where
    import Data.List
    import System.Random
    import Debug.Trace

    type Domino = (Int,Int)

    type Hand = [Domino]

    type Board = [Domino]

    data End = L | R deriving (Eq, Show)

    canPlay :: Domino -> End -> Board -> Bool
    canPlay domino end [] = True
    {-- check if domino can be played on either end by seeing 
        if the left or right pips match the end of another domino --}
    canPlay (l,r) L ((pips,_):_)
        | (l == pips || r == pips) = True
        | otherwise = False
    canPlay (l,r) R board
        | (l == pips || r == pips) = True
        | otherwise = False
        where
            (_,pips) = last board

    {-- hand is blocked if the player has no dominoes in a hand or if 
        they can't play on L/r end and rest of dominoes are blocked --}
    blocked :: Hand -> Board -> Bool
    blocked [] board = True
    blocked (d:ds) board = not (canPlay d L board) && not (canPlay d R board) && blocked ds board

    -- check if a domino is played by seeing if it is a part of the board
    played :: Domino -> Board -> Bool
    played (_,_) [] = False
    played (l,r) board = elem (l,r) board && elem (r,l) board

    -- get all the possible plays given the current hand and board
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

    {-- play a domino given that it can be played on the current board
        otherwise don't play the domino --}
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

    -- get score of a given board using the scoreConverter function
    scoreBoard :: Board -> Int
    scoreBoard board@((l,r):_)
        | l == r = scoreConverter (l+r+rightendPips)
        | otherwise = scoreConverter (l+rightendPips)
        where
            (_,rightendPips) = last board

    {-- given board and n, return all the dominoes not already played which would give a score of n and the end 
        at which to play it at--} 
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

    ---------------------------------------------------------- P2 -----------------------------------------------------------------

    type DomsPlayer = Hand -> Board -> (Domino, End)

    -- simplePlayer that plays the first domino in hand that will go
    simplePlayer :: DomsPlayer
    simplePlayer (domino:rest) board
        | playDom domino board L /= Nothing = (domino,L)
        | playDom domino board R /= Nothing = (domino,R) 
        | otherwise = simplePlayer rest board

    -- shuffle the dominoes given a seed n
    shuffleDoms :: StdGen -> [Domino]
    cmp (x1,y1) (x2,y2) = compare y1 y2
    shuffleDoms gen = [leftPips | (leftPips, n) <- sortBy cmp (zip domSet (randoms gen :: [Int]))]

    -- Player data type to keep track of turns
    data Player = P1 | P2 deriving (Eq, Show)

    -- play a round of dominoes, alternate recursively until both players are blocked
    playDomsRound :: DomsPlayer -> DomsPlayer -> Int -> (Int, Int)
    playDomsRound p1 p2 seed 
        = playDomsRound' p1 p2 P1 (h1, h2, [], (0,0))
            where
                h1 = take 7 (shuffleDoms (mkStdGen seed))
                h2 = take 7 (drop 7 (shuffleDoms (mkStdGen seed)))
                playDomsRound' p1 p2 turn gameState@(h1, h2, board, (s1,s2))
                    | blocked h1 board && blocked h2 board = (s1,s2)
                    | turn == P1 && blocked h1 board = playDomsRound' p1 p2 P2 gameState 
                    | turn == P2 && blocked h2 board = playDomsRound' p1 p2 P1 gameState
                    | otherwise = playDomsRound' p1 p2 P2 newGameState
                    where
                        (domino, end) 
                            | turn == P1 = p1 h1 board
                            | turn == P2 = p2 h2 board
                        Just newBoard = playDom domino board end
                        newGameState
                            | turn == P1 = (h1\\[domino], h2, newBoard, (s1+(scoreBoard newBoard), s2))
                            | turn == P2 = (h1, h2\\[domino], newBoard, (s1, (s2 + (scoreBoard newBoard))))

    domSet = [(l,r) | l <- [0..6], r <- [0..6]]