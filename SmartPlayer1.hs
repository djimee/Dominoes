module SmartPlayer1 where
    import System.Random
    import Data.List
    import Debug.Trace
    import DomsMatch

    -- get the current board using the history
    getBoard :: DominoBoard -> [Domino]
    getBoard InitBoard = []
    getBoard (Board d1 d2 history) = [d1 | (d1, _, _) <- history]

    -- get the hand of the opponent - assuming opponent is P2
    getOpponentHand :: DominoBoard -> [Domino]
    getOpponentHand InitBoard = []
    getOpponentHand (Board d1 d2 history) = [d1 | (d1, P2, _) <- history]

    -- get the score playing a domino on a certain end would give
    domScore :: Domino -> DominoBoard -> End -> Int
    domScore domino board end = scoreBoard boardScore
        where
            Just boardScore = playDom P1 domino board end 

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
        where oppHand = getOpponentHand board