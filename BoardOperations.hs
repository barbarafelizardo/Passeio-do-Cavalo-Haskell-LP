-- em haskell, não dá para mudar elementos de uma estrutura de dados que vc já criou
-- sempre que quisermos atualizar o valor na matriz, temos que criar uma nova 
-- aqui vamos fazer as funções de lidar com o tabuleiro/matriz

module BoardOperations (replace, updateBoard, createChessboard) where

-- faz ima matriz de zeros do tamanho do tabuleiro
createChessboard :: Int -> Int -> [[Int]]
createChessboard w h = 
    replicate h (replicate w 0)

-- substitui uma linha (lista) por outra mudando só no índice que queremos
replace :: Int -> a -> [a] -> [a]  --qualquer letra minuscula serve como placeholder de uma variável de tipo genérico
replace index value list = 
    if index == 0
        then value : tail list
    else
        let (beg, end) = splitAt index list
        in beg ++ [value] ++ tail end

-- cria um novo tabuleiro mudando só a célula que a gente queria
updateBoard :: [[a]] -> Int -> Int -> a -> [[a]]
updateBoard board lineIndex colIndex value = 
    let (matrixBeg, matrixEnd) = splitAt lineIndex board 
        currentLine = head matrixEnd
        updatedLine = replace colIndex value currentLine
    in matrixBeg ++ [updatedLine] ++ tail matrixEnd