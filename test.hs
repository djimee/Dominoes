    -- find highest scoring domino 
    highestScoringDom :: Hand -> DominoBoard -> (Domino, End)
    highestScoringDom h InitBoard
        | elem (5,4) h = ((5,4), L) -- use (5,4) if you have first drop, as it gives score of 3, and max reply is 2
        | not(null(head h, L)) = (head h, L)
    highestScoringDom h b =
        let
            possPlaysTuple = possPlays h b
            ld = fst (possPlaysTuple) 
            rd = snd (possPlaysTuple)
            lscores = zip ld (map (\d->(domScore d b L)) ld) -- [(Dom, score)]
            rscores = zip rd (map (\d->(domScore d b R)) rd)
            (lb,ls) = if (not(null lscores)) then (maximumBy (comparing snd) lscores) else ((0,0),-1) -- can't be chosen
            (rb,rs) = if (not(null rscores)) then (maximumBy (comparing snd) rscores) else ((0,0),-1)
        in
            if (ls>=rs) then (lb,L) else (rb,R)