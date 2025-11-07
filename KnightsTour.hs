module KnightsTour(isMovementValid, isStartReachable, knightsTourProblem) where

isMovementValid :: [Int] -> [[Int]] -> Int -> Int -> Bool
isMovementValid proxMov cBoard w h =
    let isInBoard = ((head proxMov) >= 0) && ((proxMov !! 1) >= 0) && ((head proxMov) <= (h - 1)) && ((proxMov !! 1) <= (w - 1))
    in isInBoard && (((cBoard !! (head proxMov)) !! (proxMov !! 1)) == 0)


isStartReachable :: [Int] -> [Int] -> [[Int]] -> Bool
isStartReachable cPos sPos posMov =
    let nextMovements = map (\move -> [head cPos + head move, cPos !! 1 + move !! 1]) posMov
    in any (\move -> move == sPos) nextMovements


knightsTourProblem :: [[Int]] -> [Int] -> [Int] -> Int -> [Int] Bool
knightsTourProblem cBoard cPos sPos sCounter posMov =
    let
        rowIndex = head cPos
        colIndex = cPos !! 1
        w = length (head chBoard)
        h = length chBoard
        chBoard = updateBoard cBoard rowIndex colIndex sCounter
    in
        if (sCounter == (w * h))
            then (isStartReachable cPos sPos posMov)
        else
            let
                nextMovements = map (\move -> [head cPos + head move, cPos !! 1 + move !! 1]) posMov
                validNextPositions = filter (\pos -> isMovementValid pos chBoard w h) nextMovements
                tryThisWay nextPos = knightsTourProblem chBoard nextPos sPos (sCounter + 1) posMov
            in any tryOneMove validNextPositions
