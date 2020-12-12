module DomsAssignment where
    import DomsMatch
    import System.Random
    import Data.Function 
    import Data.List
    import Data.Ord
    import Data.Tuple
    import Debug.Trace

    -- defensive player that has a focus on blocking the opponent and preventing them from winning

    highestScoringPlayer :: DomsPlayer
    highestScoringPlayer hand board player scores = playHighestScoringDomino hand board

    -- offensive player that plays highest scoring domino, that focuses on winning ASAP
    offensivePlayer :: DomsPlayer
    offensivePlayer hand board player scores 
        | scorePlayer == 53 && not playerCanWin = playNoBustDomino hand board player scores
        | playerCanWin = playWinner hand board player scores
        | otherwise = playHighestScoringDomino hand board
            where 
                playerCanWin = canGet61 hand board scorePlayer
                scorePlayer = getScore player scores

    -- defensive player that focuses on blocking the opponent

    -- get the current board using the history
    getBoard :: DominoBoard -> [Domino]
    getBoard InitBoard = []
    getBoard (Board d1 d2 history) = [d1 | (d1, _, _) <- history]

    -- get the score playing a domino on a certain end would give
    domScore :: Domino -> DominoBoard -> End -> Int
    domScore domino board end = scoreBoard updatedBoard
        where
            Just updatedBoard = playDom P1 domino board end

    -- get the hand of the opponent - assuming opponent is P2
    getOpponentHand :: DominoBoard -> [Domino]
    getOpponentHand InitBoard = []
    getOpponentHand (Board d1 d2 history) = [d1 | (d1, P2, _) <- history]
    
    -- get the score of a given player
    getScore :: Player -> Scores -> Int
    getScore player scores@(scoreP1, scoreP2)
        | player == P1 = scoreP1
        | player == P2 = scoreP2

    -- check if the player can get 59 given their current score
    canGet59 :: Hand -> DominoBoard -> Int -> Bool
    canGet59 [] board score = False
    canGet59 _ InitBoard _ = False
    canGet59 (d:ds) board score 
        | canPlay d R board && totalRightScore == 59 = True
        | canPlay d L board && totalLeftScore == 59 = True
        | otherwise = canGet59 ds board score
            where
                totalRightScore = score + (domScore d board R)
                totalLeftScore = score + (domScore d board L)

    -- given the hand, board and score of the player, can they win? (reach 61)
    canGet61 :: Hand -> DominoBoard -> Int -> Bool
    canGet61 [] board score = False
    canGet61 _ InitBoard _ = False
    canGet61 (d:ds) board score 
        | canPlay d R board && totalRightScore == 61 = True
        | canPlay d L board && totalLeftScore == 61 = True
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
        | possPlaysL == [] = canBlockOpponent hand opponentHand R board
        | possPlaysR == [] = canBlockOpponent hand opponentHand L board
        | otherwise = False
            where 
                possPlaysTuple = possPlays opponentHand board
                (possPlaysL, possPlaysR) = possPlaysTuple
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

    -- if player has the majority of one particular spot value, then play it

    -- play a domino that gets the players' score to 59 - if it is possible for them to reach 59
    play59Domino :: Hand -> DominoBoard -> Player -> Scores -> (Domino, End)
    play59Domino (d:ds) board player scores
        | canPlay d L board && domScore d board L + scorePlayer == 59 = (d, L) 
        | canPlay d R board && domScore d board R + scorePlayer == 59 = (d, R)
        | otherwise = play59Domino ds board player scores
            where 
                scorePlayer = getScore player scores

    -- play domino that will win the game (reach 61) - if it is possible for them to win
    playWinner :: Hand -> DominoBoard -> Player -> Scores -> (Domino, End)
    playWinner (d:ds) board player scores
        | canPlay d L board && domScore d board L + scorePlayer == 61 = (d, L) 
        | canPlay d R board && domScore d board R + scorePlayer == 61 = (d, R)
        | otherwise = playWinner ds board player scores
            where 
                scorePlayer = getScore player scores

    {-- if player is at 53, play a domino that will get less than 61 if there is no domino
    that will get the player a score of 59 --}
    playNoBustDomino :: Hand -> DominoBoard -> Player -> Scores -> (Domino, End)
    playNoBustDomino (d:ds) board player scores
        | canPlay d R board && domScore d board R + scorePlayer < 61 = (d, R)
        | canPlay d L board && domScore d board L + scorePlayer < 61 = (d, L)
        | otherwise = playNoBustDomino ds board player scores
            where 
                scorePlayer = getScore player scores
        
    -- find and play highest scoring domino, if its the first drop, use (5,4) if possible otherwise just play first domino in hand
    playHighestScoringDomino :: Hand -> DominoBoard -> (Domino, End)
    playHighestScoringDomino hand InitBoard
        | elem (5,4) hand = ((5,4), L) -- use (5,4) if you have first drop, as it gives score of 3, and max reply is 2
        | otherwise = (head hand, L) 
    playHighestScoringDomino hand board
        | leftScore >= rightScore = (leftDom, L)
        | otherwise = (rightDom, R)
        where
            (possPlaysL, possPlaysR) = possPlays hand board
            leftDoms = zip possPlaysL [domScore domino board L | domino <- possPlaysL]
            rightDoms = zip possPlaysR [domScore domino board R | domino <- possPlaysR]
            (leftDom, leftScore)
                | not (null leftDoms) = maximumBy (comparing snd) leftDoms 
                | otherwise = ((0,0),-1) -- given -1 score so that it cannot be chosen as the highest scoring domino if null
            (rightDom, rightScore)
                | not (null rightDoms) = maximumBy (comparing snd) rightDoms 
                | otherwise = ((0,0),-1)