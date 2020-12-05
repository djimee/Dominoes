-- gives set of dominoes that are in the hands of players'
handDominoes :: [Domino] -> Board -> [Domino]
handDominoes dominoes board = dominoes \\ board

-- gives list of playable dominoes given a set of dominoes, board and the end
-- in which you want to play it
playableDominoes :: [Domino] -> Board -> End -> [Domino]
playableDominoes [] board end = []
playableDominoes ((l,r):ds) board@((pips,_):_) L
    = if l == pips || r == pips
        then [(l,r)] ++ playableDominoes ds board L
    else playableDominoes ds board L
playableDominoes ((l,r):ds) board@((pips,_):_) R
    = if l == pips || r == pips
        then [(l,r)] ++ playableDominoes ds board R
    else playableDominoes ds board R
        where (_,pips) = last board
