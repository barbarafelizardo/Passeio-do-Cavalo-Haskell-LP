module FileReader( BoardInput(..), readInputFile) where

-- Estrutura de dados representando uma entrada do arquivo
data BoardInput = BoardInput
    { rows      :: Int
    , cols      :: Int
    , startRow  :: Int
    , startCol  :: Int
    } deriving (Show, Eq)

-- Função principal: lê o arquivo e retorna lista de entradas válidas
readInputFile :: FilePath -> IO [BoardInput]
readInputFile filePath = do
    contents <- readFile filePath
    let linesOfFile = lines contents
    return (foldr parseLine [] linesOfFile)

-- Converte uma linha em BoardInput se for válida
parseLine :: String -> [BoardInput] -> [BoardInput]
parseLine line acc =
    case mapM readIntMaybe (words line) of
        Just [r, c, sr, sc] -> BoardInput r c sr sc : acc
        _                   -> acc

-- Converte string para Maybe Int
readIntMaybe :: String -> Maybe Int
readIntMaybe str =
    case reads str of
        [(n, "")] -> Just n
        _         -> Nothing
