import BoardOperations
import Data.IntMap (update)
import KnightsTour
import FileReader (readInputFile, BoardInput (startCol, rows, BoardInput))

possibleMovements :: [[Int]]
possibleMovements =
    [[2, 1], [2, -1], [-2, 1], [-2, -1],
     [1, 2], [1, -2], [-1, 2], [-1, -2]]

main :: IO ()
main = do
    inputs <- readInputFile "entrada.txt"
    mapM_ processInput inputs

processInput :: BoardInput -> IO ()
processInput (BoardInput rows cols startRow startCol) = do
    putStrLn $ "Testando linha " ++ show rows ++ ", " ++ show cols ++ ", " ++ show startRow ++ ", " ++ show startCol
    let chessboard = createChessboard cols rows
        startPos = [startRow, startCol]
        stepCounter = 1
        result = knightsTourProblem chessboard startPos startPos stepCounter possibleMovements
    putStrLn $ "Resultado: " ++ show result ++ "\n"




    






