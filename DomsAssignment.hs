module DomsAssignment where
    import DomsMatch
    import System.Random
    import Data.Function 
    import Data.List
    import Data.Ord
    import Data.Tuple
    import Debug.Trace

    -- offensive player that plays highest scoring domino, that focuses on winning ASAP
    offensivePlayer :: DomsPlayer
    offensivePlayer hand board player scores 
        | scorePlayer >= 51 && canGet59 hand board scorePlayer && not playerCanWin = play59Domino hand board player scores
        | scorePlayer > 53 && canPlayNoBustDomino hand board scorePlayer && not playerCanWin = playNoBustDomino hand board player scores
        | playerCanWin = playWinner hand board player scores
        | otherwise = playHighestScoringDomino hand board
            where 
                playerCanWin = canGet61 hand board scorePlayer
                scorePlayer = if player == P1 then getScore player scores else getOppScore player scores

    -- defensive player that focuses on blocking the opponent
    defensivePlayer :: DomsPlayer
    defensivePlayer hand board player scores
        | playerCanWin = playWinner hand board player scores
        | otherwise = playRandom hand board player
            where
                playerCanWin = canGet61 hand board scorePlayer
                scorePlayer = if player == P1 then getScore player scores else getOppScore player scores

    -- get the score playing a domino on a certain end would give
    getDomScore :: Domino -> DominoBoard -> End -> Int
    getDomScore domino board end = scoreBoard updatedBoard
        where
            Just updatedBoard = playDom P1 domino board end

    -- get the score of the player (not opponent) given the scores of both players
    getScore :: Player -> Scores -> Int
    getScore player scores@(scoreP1, scoreP2) = if player == P1 then scoreP1 else scoreP2

    -- get the score of the opponent given the scores of both players
    getOppScore :: Player -> Scores -> Int
    getOppScore player scores@(scoreP1, scoreP2) = if player == P2 then scoreP2 else scoreP1

    {-- get remaining dominoes given the players hand - not taking into account what the player 
        thinks the opponent has - i.e. what to guard against --}
    getRemainingDominoes :: Hand -> History -> [Domino]
    getRemainingDominoes [] history = domSet
    getRemainingDominoes hand history = [d | d <- domSet, not (d `elem` (domino ++ hand))]
        where 
            (domino, player, moveNum) = unzip3 history

    -- get the current board using the history
    getBoard :: DominoBoard -> [Domino]
    getBoard InitBoard = []
    getBoard (Board d1 d2 history) = [d1 | (d1, _, _) <- history]

    {-- recurse over the history to find two consecutive times the same player has placed a domino to find
    the move numbers at which the opponent knocked --}
    getOpponentKnocks :: History -> Player -> [Int]
    getOpponentKnocks history@((fstDomino, P1, fstMoveNum):rest@((sndDomino, P2, sndMoveNum):hists)) player
        | player == P1 && player == P2 = fstMoveNum : getOpponentKnocks rest player
        | otherwise = getOpponentKnocks rest player

    {-- get the states of the board for the move numbers at which the opponent knocked given the history and list of move 
        numbers at which the opponent knocked --}  
    getKnockBoards :: History -> [Int] -> [History]
    getKnockBoards [] _ = []
    getKnockBoards _ [] = []
    getKnockBoards history (k:ks) = [(d, p, moveNum) | (d, p, moveNum) <- history, moveNum == k] : getKnockBoards history ks

    -- get the dominoes that the opponent cannot have given the players hand and the history of opponent knocks
    nonOpponentHand :: Hand -> [History] -> [Domino]
    nonOpponentHand hand historyList = hand ++ possibleDominoes
        where 
            noneHandValues = nub ([l | [((l,r),p, n)] <- historyList] ++ [r | [((l,r),p, n)] <- historyList])
            possibleDominoes = [(x,y) | x <- noneHandValues, y <- [0..6]]

    guessOpponentHand :: Player -> Hand -> History -> Hand
    guessOpponentHand player hand history =
        let 
            knocks = getOpponentKnocks history player 
            knockBoard = getKnockBoards history knocks
            nonHand = nonOpponentHand hand knockBoard
            historyDoms = [d | (d,p,n) <- history] ++ nonHand
        in 
            filter (\x -> notElem x historyDoms) domSet

    -- check for a dangerous domino e.g (6,6) if you don't have any more 6s in your hand, returns true if there is a dangerous domino
    {--checkDanger :: Hand -> Maybe Domino
    checkDanger [(l,r):ds]
        | l == r && --}

    {--
    -- gets the hand and turns it into a list - for counting
    dominoList ::  [(a, a)] -> [a]
    dominoList handDominoes = concat [[a,b] | (a, b) <- handDominoes]

    -- updates the elements of a list at a given position
    updateDoms :: (Num a) => Int -> [a] -> [a]
    updateDoms n xs = take n xs ++ [(xs !! n) + 1] ++ drop (n + 1) xs

    -- performs count of occurences of each spot value
    countOccurrences :: [(Int, Int)] -> [Int]
    countOccurrences xs = go (dominoList xs) count
        where 
            go [] count = count
            go (x:xs) count = go xs (updateDoms x count)
            count = replicate 7 0       

    -- check for majority of one particuar spot value
    majoritySpotValue :: Hand -> Bool
    majoritySpotValue hand = length highSpotValues /= 0
        where 
            frequencyList = countOccurrences hand
            highSpotValues = [spotValue | spotValue <- frequencyList, spotValue > 10]

    -- if player has the majority of one particular spot value, then play it
    playMajoritySpotValue :: 
    --}

    -- check if the player has a weak hand i.e. the highest scoring domino will give a score of 0 given the hand and board
    weakHand :: Hand -> DominoBoard -> Bool
    weakHand hand board
        | leftScore == 0 || rightScore == 0 = True -- check if the highest scoring domino on either side is 0
        | otherwise = False
            where
                (possPlaysL, possPlaysR) = possPlays hand board
                leftDoms = zip possPlaysL [getDomScore domino board L | domino <- possPlaysL]
                rightDoms = zip possPlaysR [getDomScore domino board R | domino <- possPlaysR]
                (leftDom, leftScore)
                    | not (null leftDoms) = maximumBy (comparing snd) leftDoms 
                    | otherwise = ((0,0),-1) -- given -1 score so that it cannot be chosen as the highest scoring domino if null
                (rightDom, rightScore)
                    | not (null rightDoms) = maximumBy (comparing snd) rightDoms 
                    | otherwise = ((0,0),-1)

    -- check if the player can get 59 given their current hand, board and current score
    canGet59 :: Hand -> DominoBoard -> Int -> Bool
    canGet59 [] board score = False
    canGet59 _ InitBoard _ = False
    canGet59 (d:ds) board score 
        | canPlay d R board && totalRightScore == 59 = True
        | canPlay d L board && totalLeftScore == 59 = True
        | otherwise = canGet59 ds board score
            where
                totalRightScore = score + (getDomScore d board R)
                totalLeftScore = score + (getDomScore d board L)

    -- given the hand, board and score of the player, can they win? (reach 61)
    canGet61 :: Hand -> DominoBoard -> Int -> Bool
    canGet61 [] board score = False
    canGet61 _ InitBoard _ = False
    canGet61 (d:ds) board score 
        | canPlay d R board && totalRightScore == 61 = True
        | canPlay d L board && totalLeftScore == 61 = True
        | otherwise = canGet61 ds board score
            where
                totalRightScore = score + (getDomScore d board R)
                totalLeftScore = score + (getDomScore d board L)

    {-- given the hand, board and current score, check if the player can play a domino that will give a score
        under 61 - used when score > 53 --}
    canPlayNoBustDomino :: Hand -> DominoBoard -> Int -> Bool
    canPlayNoBustDomino [] board score = False
    canPlayNoBustDomino _ InitBoard _ = False
    canPlayNoBustDomino (d:ds) board score 
        | canPlay d R board && totalRightScore < 61 = True
        | canPlay d L board && totalLeftScore < 61 = True
        | otherwise = canPlayNoBustDomino ds board score
            where
                totalRightScore = score + (getDomScore d board R)
                totalLeftScore = score + (getDomScore d board L)

    -- play a domino that gets the players' score to 59 - if it is possible for them to reach 59
    play59Domino :: Hand -> DominoBoard -> Player -> Scores -> (Domino, End)
    play59Domino (d:ds) board player scores
        | canPlay d L board && getDomScore d board L + scorePlayer == 59 = (d, L) 
        | canPlay d R board && getDomScore d board R + scorePlayer == 59 = (d, R)
        | otherwise = play59Domino ds board player scores
            where 
                scorePlayer = if player == P1 then getScore player scores else getOppScore player scores

    -- play domino that will win the game (reach 61) - if it is possible for them to win
    playWinner :: Hand -> DominoBoard -> Player -> Scores -> (Domino, End)
    playWinner (d:ds) board player scores
        | canPlay d L board && getDomScore d board L + scorePlayer == 61 = (d, L) -- check domino can be played and if score will be 61 after playing
        | canPlay d R board && getDomScore d board R + scorePlayer == 61 = (d, R)
        | otherwise = playWinner ds board player scores
            where 
                scorePlayer = if player == P1 then getScore player scores else getOppScore player scores

    -- if player is at 53, play a domino that will get less than 61 if there is no domino that will get the player a score of 59
    playNoBustDomino :: Hand -> DominoBoard -> Player -> Scores -> (Domino, End)
    playNoBustDomino (d:ds) board player scores
        | canPlay d R board && getDomScore d board R + scorePlayer < 61 = (d, R) -- check domino can be played and score will be less than 61 after playing
        | canPlay d L board && getDomScore d board L + scorePlayer < 61 = (d, L)
        | otherwise = playNoBustDomino ds board player scores
            where 
                scorePlayer = if player == P1 then getScore player scores else getOppScore player scores
        
    {-- find and play highest scoring domino, if its the first drop, use (5,4) if possible 
        otherwise just play first domino in hand --}
    playHighestScoringDomino :: Hand -> DominoBoard -> (Domino, End)
    playHighestScoringDomino hand InitBoard
        | elem (5,4) hand = ((5,4), L) -- use (5,4) if you have first drop, as it gives score of 3, and max reply is 2
        | otherwise = (head hand, L) 
    playHighestScoringDomino hand board
        | leftScore >= rightScore = (leftDom, L)
        | otherwise = (rightDom, R)
        where
            (possPlaysL, possPlaysR) = possPlays hand board
            leftDoms = zip possPlaysL [getDomScore domino board L | domino <- possPlaysL]
            rightDoms = zip possPlaysR [getDomScore domino board R | domino <- possPlaysR]
            (leftDom, leftScore)
                | not (null leftDoms) = maximumBy (comparing snd) leftDoms 
                | otherwise = ((0,0),-1) -- given -1 score so that it cannot be chosen as the highest scoring domino if null
            (rightDom, rightScore)
                | not (null rightDoms) = maximumBy (comparing snd) rightDoms 
                | otherwise = ((0,0),-1)
    
    {-- play random domino function - low level function that just plays first domino that will go 
        (from DomsMatch) --}
    playRandom :: Hand -> DominoBoard -> Player -> (Domino, End)
    playRandom (d:ds) board player
        | leftBoard /= Nothing = (d, L)
        | rightBoard /= Nothing = (d, R)
        | otherwise = playRandom ds board player
            where
                leftBoard = playDom player d board L -- check if domino will go on the left end
                rightBoard = playDom player d board R 