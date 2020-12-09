module DomsAssignment where
    import System.Random
    import Data.Function 
    import Data.List
    import Debug.Trace
    import DomsMatch
    import Data.Ord

    -- defensive player that has a focus on blocking the opponent and preventing them from winning

    -- offensive player that plays highest scoring domino, with a much larger focus on winning
    offensivePlayer :: DomsPlayer
    offensivePlayer hand board player score = highestScoringDom hand board

    -- get the current board using the history
    getBoard :: DominoBoard -> [Domino]
    getBoard InitBoard = []
    getBoard (Board d1 d2 history) = [d1 | (d1, _, _) <- history]

    -- get the score playing a domino on a certain end would give
    domScore :: Domino -> DominoBoard -> End -> Int
    domScore domino board end = scoreBoard boardScore
        where
            Just boardScore = playDom P1 domino board end 

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

    -- find highest scoring domino, if its the first drop, use (5,4) if possible otherwise........... 
    highestScoringDom :: Hand -> DominoBoard -> (Domino, End)
    highestScoringDom hand InitBoard
        | elem (5,4) hand = ((5,4), L) -- use (5,4) if you have first drop, as it gives score of 3, and max reply is 2
        | otherwise = (head hand, L)
    highestScoringDom hand board = 
        if leftScore >= rightScore then (leftDom, L) else (rightDom, R)
        where
            possPlaysTuple = possPlays hand board
            possPlaysL = fst (possPlaysTuple) 
            possPlaysR = snd (possPlaysTuple)
            leftDoms = zip possPlaysL [domScore domino board L | domino <- possPlaysL]
            rightDoms = zip possPlaysR [domScore domino board R | domino <- possPlaysR]
            (leftDom, leftScore) = if (not (null leftDoms)) then maximumBy (comparing snd) leftDoms else ((0,0), -1)
            (rightDom, rightScore) = if (not (null rightDoms)) then maximumBy (comparing snd) rightDoms else ((0,0), -1)
        
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

    -- play domino that will win the game (reach 61) - if it is possible for them to win

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