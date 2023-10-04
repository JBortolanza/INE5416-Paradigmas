type Board = [[Int]] -- Declara a estrutura de dados Board, uma lista de listas com inteiros (matriz), que contem o jogo de sudoku

-- Funcao que faz o update na matriz caso um numero valido seja encontrado:
updateBoard :: Int -> Int -> Int -> Board -> Board -- Recebe a linha, coluna e numero que devera ser substituido no elemento respectivo, matriz com o jogo e retorna a nova matriz
updateBoard row col num board =
    take row board ++ [take col (board !! row) ++ [num] ++ drop (col + 1) (board !! row)] ++ drop (row + 1) board

-- Function to check if a number is valid in a cell
is_valid :: Board -> Int -> Int -> Int -> Bool
is_valid board row col num =
    notElem num (board !! row) &&
    notElem num (map (!! col) board) &&
    notElem num (concatMap (take 3 . drop (col `div` 3 * 3)) (take 3 $ drop (row `div` 3 * 3) board))

solve :: Int -> Int -> Board -> Maybe Board
solve row col board
    | row == 9 = Just board  -- We've reached the end, solution found
    | col == 9 = solve (row + 1) 0 board  -- Move to the next row
    | otherwise = 
        let validNums = filter (is_valid board row col) [1..9]
        in  foldr (\num acc -> case acc of
                Just _ -> acc
                Nothing -> solve row (col + 1) (updateBoard row col num board)
            ) Nothing validNums

main :: IO ()
main = do
    let emptyBoard = replicate 9 (replicate 9 0)
    case solve 0 0 emptyBoard of
        Just solution -> putStrLn $ unlines $ map unwords $ map (map show) solution
        Nothing -> putStrLn "No solution exists."