import BoardOperations
import Data.IntMap (update)

currentX :: Int
currentY :: Int

width :: Int
height :: Int

startX :: Int
startY :: Int
-- ler documento txt e setar as variáveis acima, checar se são válidas antes, se não, falar que algum dos dados é inválido e pular para a próxima linha
currentPos :: [Int]
currentPos = [currentX, currentY] 

startPos :: [Int, Int]
startPost = [startX, startY]

stepCounter :: Int
stepCounter = 1

possibleMovements :: [[Int]]
possibleMovements = [[2, 1], [2, -1], [-2, 1], [-2, -1], [1, 2], [1, -2], [-1, 2], [-1, -2]]

chessboard :: [[Int]]


createChessboard :: [[Int]] -> Int -> Int -> [[Int]]

createChessboard w h = 
    replicate h (replicate w 0)


chessboard = createChessboard width height


isMovementValid :: [Int] -> [[Int]] -> Int -> Int -> Bool
isMovementValid proxMov cBoard w h = 
    let isInBoard = ((proxMov !! 0) >= 0) && ((proxMov !! 1) >= 0) && ((proxMov !! 0) <= (h - 1)) && ((proxMov !! 1) <= (w - 1))
    in isInBoard && (((cBoard !! (proxMov !! 0)) !! (proxMov !! 1)) == 0)


isStartReachable :: [Int] -> [Int] -> [Int] -> Bool
isStartReachable cPos sPos possMov = 
    if (length posMov >= 1)  
        then if (zipWith (+) cPos (take 1 possMov) == sPos)
                then True
            else isStartReachable cPos sPos (drop 1 possMov)
    else False


knightsTourProblem :: [[Int]] -> [Int] -> [Int] -> Int -> [Int] Bool
knightsTourProblem :: cBoard cPos sPos sCounter posMov =
    let 
        rowIndex = cPos !! 0
        colIndex = cPos !! 1
    in
        chBoard :: [[Int]]
        chBoard = updateBoard cBoard rowIndex colIndex sCounter

        if (sCounter == (length (chBoard !! 0) * length chBoard))
            then if (isStartReachable cPos sPos possibleMovements)
                then True
            else False
        --else if 



    






