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

--possPlays :: Hand -> Board -> ([Domino], [Domino])
--possPlays [] board = null 

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

