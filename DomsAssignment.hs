module SmartPlayer1 where
    import System.Random
    import Data.List
    import Debug.Trace
    import DomsMatch

    -- get the current board using the history
    getBoard :: DominoBoard -> [Domino]
    getBoard InitBoard = []
    getBoard (Board d1 d2 history) = [d1 | (d1, _, _) <- history]

    {-- if the player has first drop and has the domino (5,4), play it
        because it scores 3 and has a maximum reply of 2 --}
    firstDropPlay :: Player -> Hand -> DominoBoard -> Maybe DominoBoard
    firstDropPlay player hand InitBoard
        | elem (5,4) hand = playDom player (5,4) InitBoard L -- end doesn't matter as it's first drop
        | otherwise = Nothing

    -- get the hand of the opponent - assuming opponent is P2
    getOpponentHand :: DominoBoard -> [Domino]
    getOpponentHand InitBoard = []
    getOpponentHand (Board d1 d2 history) = [d1 | (d1, P2, _) <- history]

    -- get the score playing a domino on a certain end would give
    domScore :: Domino -> DominoBoard -> End -> Int
    domScore domino board end = scoreBoard boardScore
        where
            Just boardScore = playDom P1 domino board end 
    
    -- get the score of a given player
    getScore :: Player -> Scores -> Int
    getScore player scores@(scoreP1, scoreP2)
        | player == P1 = scoreP1
        | player == P2 = scoreP2

    -- check for majority of one particuar spot value

    -- if player has the majority of one particular spot value, then play it

    -- if player is at 53, get a set of dominoes the player can play so that they don't
    -- go over 61

    -- given the hand, board and score of the player, can they win? (reach 61)
    canGet61 :: Hand -> DominoBoard -> Int -> Bool
    canGet61 _ InitBoard _ = False
    canGet61 [] board score = False
    canGet61 (d:ds) board score 
        | (canPlay d R board) && (score + (domScore d board R) == 61) = True
        | (canPlay d L board) && (score + (domScore d board L) == 61) = True
        | otherwise = canGet61 ds board score

    -- given the opponents' hand, board and score, can they win?
    canOpponentGet61 :: Hand -> DominoBoard -> Int -> Bool
    canOpponentGet61 _ InitBoard _ = False
    canOpponentGet61 [] board score = False
    canOpponentGet61 oppHand board oppScore = canGet61 oppHand board oppScore 
        where 
            oppHand = getOpponentHand board

    -- check if the opponent can be blocked

    -- if opponent can win, block them if it is possible

    -- get highest scoring domino given the hand and current board