-- em haskell, não dá para mudar elementos de uma estrutura de dados que vc já criou
-- sempre que quisermos atualizar o valor na matriz, temos que criar uma nova 
-- aqui vamos fazer as funções de lidar com o tabuleiro/matriz

module BoardOperations (..) where

replace :: Int -> a -> [a] -> [a]  --qualquer letra minuscula serve como placeholder de uma variável de tipo genérico
-- valor p mudar -- indice -- lista

updateBoard :: [[a]] -> Int -> Int -> a -> [[a]]
-- tabuleiro -- indice da linha -- indice da coluna -- valor p mudar