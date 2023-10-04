
type Board = [[Int]] -- Declara a estrutura de dados Board, uma lista de listas com inteiros (matriz), que contem o jogo de sudoku
type Comparison = (Int, Int, Int, Int) -- Declara a estrutura de dados Comparison, uma tupla com inteiros que contem uma comparacao de > no tabuleiro

-- Funcao que faz o update na matriz caso um numero valido seja encontrado:
updateBoard :: Int -> Int -> Int -> Board -> Board -- Recebe a linha, coluna e numero que devera ser substituido no elemento respectivo, matriz com o jogo e retorna a nova matriz
updateBoard row col num board =
    take row board ++ [take col (board !! row) ++ [num] ++ drop (col + 1) (board !! row)] ++ drop (row + 1) board -- Funcionamento dessa linha comentado com mais detalhes no relatorio. Eh onde o update eh feito

-- Funcao responsavel por checar se as condicoes do Vergleich sudoku foram respeitadas, tambem em maior detalhe no relatorio
checkComparisons :: Board -> Int -> Int -> Int -> Comparison -> Bool
checkComparisons board row col num (r1, c1, r2, c2) 
    | row == r1 && col == c1 && num < board !! r2 !! c2 && board !! r2 !! c2 /= 0 = False -- Verifica se a comparacao atual se refere ao item desejado, se sim verifica se este eh o primeiro deles, caso seja verifica se eh maior que o segundo (caso nao seja 0)
    | row == r2 && col == c2 && num > board !! r1 !! c1 && board !! r1 !! c1 /= 0 = False -- Faz o mesmo que a linha de cima, verificando se o item eh o segundo da comp atual
    | otherwise = True -- Retorna true para todos os casos que nao se aplicam ao item verificado por validate

-- Funcao que verifica se o numero escolhido eh valido para a celula, verifica as regras normais do sudoku e chama checkComparisons para verificar regras adicionais
validate :: Board -> Int -> Int -> Int -> [Comparison] -> Bool
validate board row col num comparisons =
    all (checkComparisons board row col num) comparisons && -- Verificacao das regras adicionais
    notElem num (board !! row) && -- Verifica se o numero ja esta presente na linha
    notElem num (map (!! col) board) && -- Verifica se o numero ja esta presente na coluna
    notElem num (concatMap (take 3 . drop (col `div` 3 * 3)) (take 3 $ drop (row `div` 3 * 3) board)) -- Verifica se o numero ja esta presente na regiao

-- Funcao principal do programa, usada para resolver o jogo via recursao (backtracking)
solve :: Int -> Int -> Board -> [Comparison] -> Maybe Board
solve row col board comparisons -- Recebe como parametros a posicao inicial (nesse caso 0,0) e continua a partir dai
    | row == 9 = Just board -- Caso a linha seja igual a 9 significa que o programa ja passou por todos os elementos e encontrou uma solucao valida, retonando a Board
    | col == 9 = solve (row + 1) 0 board comparisons -- Se a coluna for igual a 9 significa que chegou ao final da linha, adicionando 1 ao seu contador e seguindo em frente com recursao
    | otherwise = -- Caso principal do problema, onde o numero que vai em cada posicao eh definido
        let validNums = filter (\num -> validate board row col num comparisons) [1..9] -- Funcao filter aplica a validacao para todos os possiveis numeros que podem preencher a dada posicao, retornando uma lista.
        in  foldr (\num possibleBoard -> case possibleBoard of -- Essa lista eh usada pela funcao foldr que originalmente serve para acumular valores de uma lista, nesse caso esta servindo como um iterador pelos possiveis numeros para a posicao
                Just _ -> possibleBoard
                Nothing -> solve row (col + 1) (updateBoard row col num board) comparisons
            ) Nothing validNums

main :: IO ()
main = do
    let emptyBoard = replicate 9 (replicate 9 0) -- Cria o tabuleiro com 0's 
    -- Lista com as comparacoes. Sao ordenadas de modo que os dois primeiros numeros sejam a referencia do primeiro item e os outros do segundo, a comparacao sempre sera primeiro > segundo
    let comparisons = [(1, 0, 0, 0),(0, 1, 0, 0),(0, 1, 0, 2),(0, 1, 1, 1),(0, 2, 1, 2),(0, 3, 0, 4),(1, 3, 0, 3),(1, 4, 0, 4),(1, 5, 0, 5),(0, 5, 0, 4),(0, 6, 0, 7),(0, 6, 1, 6),(0, 7, 0, 8),(1, 7, 0, 7),(1, 0, 1, 1),(1, 0, 2, 0),(1, 1, 1, 2),(2, 1, 2, 0),(2, 1, 1, 1),(2, 2, 2, 1),(2, 2, 1, 2),(1, 3, 2, 3),(1, 4, 1, 3),(1, 4, 2, 4),(2, 3, 2, 4),(2, 5, 2, 4),(1, 6, 1, 7),(2, 6, 1, 6),(2, 6, 2, 7),(2, 7, 1, 7),(2, 7, 2, 8),(2, 8, 1, 8),(1, 8, 1, 7),(1, 8, 0, 8),(3, 0, 3, 1),(3, 0, 4, 0),(3, 1, 3, 2),(4, 0, 4, 1),(4, 0, 5, 0),(4, 1, 3, 1),(4, 1, 4, 2),(4, 1, 5, 1),(5, 0, 5, 1),(4, 2, 3, 2),(5, 2, 5, 1),(5, 2, 4, 2),(3, 4, 3, 3),(3, 3, 4, 3),(3, 4, 3, 5),(3, 5, 4, 5),(4, 4, 3, 4),(4, 4, 3, 4),(4, 4, 3, 4),(4, 4, 4, 5),(4, 4, 4, 3),(4, 4, 5, 4),(5, 3, 4, 3),(5, 3, 5, 4),(5, 5, 5, 4),(5, 5, 4, 5),(3, 7, 3, 6),(3, 7, 4, 7),(3, 8, 3, 7),(3, 8, 4, 8),(4, 6, 3, 6),(4, 7, 4, 6),(4, 7, 5, 7),(4, 8, 4, 7),(5, 6, 4, 6),(5, 7, 5, 6),(5, 8, 7, 7),(5, 8, 4, 8),(6, 1, 6, 0),(6, 1, 6, 2),(6, 1, 7, 1),(6, 0, 7, 0),(6, 2, 7, 2),(7, 1, 7, 0),(7, 1, 7, 2),(7, 1, 8, 1),(8, 0, 7, 0),(8, 0, 8, 1),(8, 1, 8, 2),(7, 2, 8, 2),(6, 4, 6, 3),(6, 5, 6, 4),(7, 3, 6, 3),(7, 3, 7, 4),(7, 4, 6, 4),(7, 4, 7, 5),(6, 5, 7, 5),(8, 3, 7, 3),(8, 3, 8, 4),(8, 4, 7, 4),(8, 4, 8, 5),(7, 5, 8, 5),(6, 6, 6, 7),(6, 6, 7, 6),(6, 8, 6, 7),(6, 8, 7, 8),(7, 7, 6, 7),(7, 7, 7, 8),(7, 7, 7, 6),(7, 7, 8, 7),(8, 6, 7, 6),(8, 7, 8, 6),(8, 7, 8, 8),(7, 8, 8, 8)]
    case solve 0 0 emptyBoard comparisons of -- Faz a primeira chamada da funcao solve e, caso seu resultado seja uma board, esta sera printada. Caso seja nothing retorna mensagem
        Just solution -> putStrLn $ unlines $ map (unwords . map show) solution
        Nothing -> putStrLn "Puzzle nao tem solucao valida."