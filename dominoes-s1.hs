{-- Sample solution to stage 1 of the pre-assignment tasks.
    There are many other possible solutions to these functions. If you have something different,
    it is not necessarily _wrong_, but do check that you have adequately covered all special cases.
    What does your test data look like? Have you considered special cases that might arise?
    
    Author: Emma Norling.
    Date: October 2020.
 --}

type Domino = (Int, Int) -- a domino is represented by a pair of integers
type Hand = [Domino] -- a hand is a list of dominoes
type Board = [Domino] -- as is a board - the head of the list is the left end and the last item is the right end
data End = L | R deriving (Eq, Show) -- left end or right end, with means of viewing/testing equality

{- canPlay checks if a particular domino can be played at a particular end of the board -}
canPlay :: Domino -> End -> Board -> Bool
-- if nothing has been played yet, any domino can be played
canPlay domino end [] = True
-- checking if can play on left end can use simple pattern matching
canPlay (m,n) L ((val,_):_) = m == val || n == val
-- but on the right end, need to get the last item in the list
canPlay (m,n) R board = m == val || n == val
                        where
                        (_, val) = last board


{- blocked goes through the dominoes in a hand and returns False as soon as it finds one
   that can be played -}
blocked :: Hand -> Board -> Bool
blocked [] board = True
blocked (d:ds) board
    = not (canPlay d L board) && not (canPlay d R board) && blocked ds board
{- an alternative approach would be to find which dominoes can be played (using the possPlays function below)
   and return True if it generates a pair of empty lists -}

{- played check if a domino has been played. Since dominos can be played in either orientation,
   it needs to check both -}
played :: Domino -> Board -> Bool
played domino [] = False
played (m,n) (domino:rest)
    | (m,n) == domino = True
    | (n,m) == domino = True
    | otherwise = played (m,n) rest

{- possPlays returns all the valid plays given the current hand and board -}
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

{- playDom attempts to place a domino at the given end of a board it returns the Just the new board
   if it is a valid play, Nothing otherwise -}
playDom :: Domino -> Board -> End -> Maybe Board
playDom domino [] end = Just [domino]
playDom (m,n) board@((val,_):_) L
    | n == val  = Just ((m,n):board) -- play domino as is
    | m == val  = Just ((n,m):board) -- need to flip the domino to play it
    | otherwise = Nothing            -- cannot play domino
playDom (m,n) board R
    | m == val  = Just (board++[(m,n)]) -- play domino as is
    | n == val  = Just (board++[(n,m)]) -- flip the domino to play it
    | otherwise = Nothing            -- cannot play this domino
      where (_,val) = last board



{- score takes the number of pips on either end of the board and
   calculates the score in the 3s and 5s scoring method -}
scoreBoard :: Board -> Int
scoreBoard board@(left:_)
    = score (pips left L + pips right R)
      where
      right = last board
      pips (m,n) end
            | m == n && left /= right = m+n -- both ends of a double count because it gets plays crosswise
            | end == L  = m
            | otherwise = n
      score n 
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

{- scoreN takes a board and a target score and returns each unplayed domino and the end to play it on
   to get that target score -}
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

hand1 :: Hand
hand1 = [(3,3),(1,2),(3,5),(4,3),(0,0),(4,4)]

domSet = [(6,6),(6,5),(6,4),(6,3),(6,2),(6,1),(6,0),
                (5,5),(5,4),(5,3),(5,2),(5,1),(5,0),
                      (4,4),(4,3),(4,2),(4,1),(4,0),
                            (3,3),(3,2),(3,1),(3,0),
                                  (2,2),(2,1),(2,0),
                                        (1,1),(1,0),
                                              (0,0)]


