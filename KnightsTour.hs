module KnightsTour(isMovementValid, isStartReachable, knightsTourProblem) where
import BoardOperations
import Data.List (sortOn)
import Debug.Trace


-- vê se o movimento está nos limites do tabuleiro e se o valor da célula dele na matriz é 0
isMovementValid :: [Int] -> [[Int]] -> Int -> Int -> Bool
isMovementValid proxMov cBoard w h =
    let isInBoard = ((head proxMov) >= 0) && ((proxMov !! 1) >= 0) && ((head proxMov) <= (h - 1)) && ((proxMov !! 1) <= (w - 1))
    in isInBoard && (((cBoard !! (head proxMov)) !! (proxMov !! 1)) == 0)

-- soma a lista de movimentos possiveis com a posição atual e vê se algum desses elementos é o início
isStartReachable :: [Int] -> [Int] -> [[Int]] -> Bool
isStartReachable cPos sPos posMov =
    let nextMovements = map (\move -> [head cPos + head move, cPos !! 1 + move !! 1]) posMov
    in any (\move -> move == sPos) nextMovements

-- gera próximos movimentos a partir de uma posição
nextFrom :: [Int] -> [[Int]] -> [[Int]]
nextFrom [r,c] posMov =
    map (\m -> [r + head m, c + (m !! 1)]) posMov
nextFrom _ _ = []

-- grau de Warnsdorff: quantos movimentos válidos existem a partir dessa posição
onwardDegree :: [Int] -> Int -> Int -> [[Int]] -> [[Int]] -> Int
onwardDegree pos w h chBoard posMov =
    let ns = nextFrom pos posMov
    in length (filter (\p -> isMovementValid p chBoard w h) ns)

-- vê se o passo que ele dá é o último, se for, vê se dá para chegar na casa que ele saiu, 
-- se não for o último, pega uma lista de movimentos válidos e tenta rodar como se cada um fosse o próximo
knightsTourProblem :: [[Int]] -> [Int] -> [Int] -> Int -> [[Int]] -> Bool
knightsTourProblem cBoard cPos sPos sCounter posMov =
    let
        rowIndex = head cPos
        colIndex = cPos !! 1

        chBoard = updateBoard cBoard rowIndex colIndex sCounter
        w = length (head chBoard)
        h = length chBoard
    in
        if sCounter == (w * h)
            then
                let result = not (isStartReachable cPos sPos posMov)
                in (result && trace ("Caminho válido encontrado: \n" ++ show cPos) True)
        else
            let
                nextMovements = nextFrom cPos posMov
                validNextPositions = filter (\pos -> isMovementValid pos chBoard w h) nextMovements

                -- ordena pelos que têm MENOS saídas depois (Warnsdorff)
                orderedNextPositions =
                    sortOn (\pos -> onwardDegree pos w h chBoard posMov) validNextPositions

                tryThisWay nextPos =
                    knightsTourProblem chBoard nextPos sPos (sCounter + 1) posMov

                result =
                    any tryThisWay orderedNextPositions
            in
                (result && trace (" " ++ show cPos) True)