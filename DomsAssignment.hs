module DomsAssignment where
    import System.Random
    import Data.Function 
    import Data.List
    import Debug.Trace
    import DomsMatch

    -- defensive player that has a focus on blocking the opponent

    -- get the current board using the history
    getBoard :: DominoBoard -> [Domino]
    getBoard InitBoard = []
    getBoard (Board d1 d2 history) = [d1 | (d1, _, _) <- history]

    -- get the score playing a domino on a certain end would give
    domScore :: Domino -> DominoBoard -> End -> Int
    domScore domino board end = scoreBoard boardScore
        where
            Just boardScore = playDom P1 domino board end 

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
    
    -- get the score of a given player
    getScore :: Player -> Scores -> Int
    getScore player scores@(scoreP1, scoreP2)
        | player == P1 = scoreP1
        | player == P2 = scoreP2

    -- check for majority of one particuar spot value

    -- if player has the majority of one particular spot value, then play it

    -- if player is at 53, get a set of dominoes the player can play so that they don't
    -- go over 61 - is this needed?

    -- find highest scoring domino 
    highestScoringDom :: Hand -> DominoBoard -> (Domino, End)
    highestScoringDom hand board = if (fst leftHighest) > (fst rightHighest) then snd leftHighest L else snd rightHighest R
        where
            sortDoms = sortBy (flip compare `on` snd)
            leftHighest = head (sortDoms (zip possPlaysL [domScore domino board L | domino <- possPlaysL]))
            rightHighest = head (sortDoms (zip possPlaysR [domScore domino board R | domino <- possPlaysR]))
            possPlaysTuple = possPlays hand board
            possPlaysL = fst (possPlaysTuple) 
            possPlaysR = snd (possPlaysTuple)
    
    -- given the hand, board and score of the player, can they win? (reach 61)
    canGet61 :: Hand -> DominoBoard -> Int -> Bool
    canGet61 _ InitBoard _ = False
    canGet61 [] board score = False
    canGet61 (d:ds) board score 
        | (canPlay d R board) && (totalRightScore == 61) = True
        | (canPlay d L board) && (totalLeftScore == 61) = True
        | otherwise = canGet61 ds board score
            where
                totalRightScore = score + (domScore d board R)
                totalLeftScore = score + (domScore d board L)

    -- given the opponents' hand, board and score, can they win?
    canOpponentGet61 :: Hand -> DominoBoard -> Int -> Bool
    canOpponentGet61 _ InitBoard _ = False
    canOpponentGet61 [] board score = False
    canOpponentGet61 opponentHand board oppScore = canGet61 opponentHand board oppScore 
        where 
            opponentHand = getOpponentHand board

    -- check if the opponent can place a domino before checking whether the opponent can be blocked
    canPlaceDomino :: Hand -> DominoBoard -> Bool
    canPlaceDomino _ InitBoard = False
    canPlaceDomino hand board@(Board d1 d2 history)
        | possPlaysTuple /= ([],[]) || cantPlay = False
        | fst possPlaysTuple == [] = canBlockOpponent hand opponentHand R board
        | snd possPlaysTuple == [] = canBlockOpponent hand opponentHand L board
        | otherwise = False
            where 
                possPlaysTuple = possPlays opponentHand board
                cantPlay = blocked opponentHand board
                opponentHand = getOpponentHand board
    
    -- check if the opponent can be blocked by placing a certain domino
    canBlockOpponent :: Hand -> Hand -> End -> DominoBoard -> Bool
    canBlockOpponent _ _ _ InitBoard  = False
    canBlockOpponent [] _ _ _ = False
    canBlockOpponent (d:ds) opponentHand end board@(Board d1 d2 history)
        | blocked opponentHand updatedBoard = True
        | otherwise = canBlockOpponent ds opponentHand end board
            where
                Just updatedBoard = playDom P1 d board end

    -- if opponent can win, block them if it is possible

    -- get highest scoring domino given the hand and current board