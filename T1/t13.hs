
type Board = [[Int]] -- Declara a estrutura de dados Board, uma lista de listas com inteiros (matriz), que contem o jogo de sudoku


-- Funcao que faz o update na matriz caso um numero valido seja encontrado:
updateBoard :: Int -> Int -> Int -> Board -> Board -- Recebe a linha, coluna e numero que devera ser substituido no elemento respectivo, matriz com o jogo e retorna a nova matriz
updateBoard row col num board =
    take row board ++ [take col (board !! row) ++ [num] ++ drop (col + 1) (board !! row)] ++ drop (row + 1) board -- Funcionamento dessa linha comentado com mais detalhes no relatorio. Eh onde o update eh feito

-- Funcao que verifica se o numero escolhido eh valido para a celula, verifica as regras normais do sudoku e chama checkComparisons para verificar regras adicionais
validate :: Board -> Int -> Int -> Int -> Bool
validate board row col num =
    notElem num (board !! row) && -- Verifica se o numero ja esta presente na linha
    notElem num (map (!! col) board) && -- Verifica se o numero ja esta presente na coluna
    notElem num (concatMap (take 3 . drop (col `div` 3 * 3)) (take 3 $ drop (row `div` 3 * 3) board)) -- Verifica se o numero ja esta presente na regiao

-- Funcao principal do programa, usada para resolver o jogo via recursao (backtracking)
solve :: Int -> Int -> Board -> Maybe Board
solve row col board -- Recebe como parametros a posicao inicial (nesse caso 0,0) e continua a partir dai
    | row == 9 = Just board -- Caso a linha seja igual a 9 significa que o programa ja passou por todos os elementos e encontrou uma solucao valida, retonando a Board
    | col == 9 = solve (row + 1) 0 board -- Se a coluna for igual a 9 significa que chegou ao final da linha, adicionando 1 ao seu contador e seguindo em frente com recursao
    | otherwise = -- Caso principal do problema, onde o numero que vai em cada posicao eh definido
        let validNums = filter (\num -> validate board row col num) [1..9] -- Funcao filter aplica a validacao para todos os possiveis numeros que podem preencher a dada posicao, retornando uma lista.
        in  foldr (\num possibleBoard -> case possibleBoard of -- Essa lista eh usada pela funcao foldr que originalmente serve para acumular valores de uma lista, nesse caso esta servindo como um iterador pelos possiveis numeros para a posicao
                Just _ -> possibleBoard
                Nothing -> solve row (col + 1) (updateBoard row col num board)
            ) Nothing validNums

main :: IO ()
main = do
    let emptyBoard = replicate 9 (replicate 9 0) -- Cria o tabuleiro com 0's 
    -- Lista com as comparacoes. Sao ordenadas de modo que os dois primeiros numeros sejam a referencia do primeiro item e os outros do segundo, a comparacao sempre sera primeiro > segundo
    case solve 0 0 emptyBoard of -- Faz a primeira chamada da funcao solve e, caso seu resultado seja uma board, esta sera printada. Caso seja nothing retorna mensagem
        Just solution -> putStrLn $ unlines $ map (unwords . map show) solution
        Nothing -> putStrLn "Puzzle nao tem solucao valida."