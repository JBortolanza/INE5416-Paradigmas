import Control.Parallel.Strategies (parMap, rseq)
type Board = [[Int]]
type Comparison = (Int, Int, Int, Int)

updateBoard :: Int -> Int -> Int -> Board -> Board
updateBoard row col num board =
    take row board ++
    [take col (board !! row) ++ [num] ++ drop (col + 1) (board !! row)] ++ drop (row + 1) board

checkConstraint :: Board -> Int -> Int -> Int -> Comparison -> Bool
checkConstraint board row col num (r1, c1, r2, c2) 
    | row == r1 && col == c1 && num < board !! r2 !! c2 && board !! r2 !! c2 /= 0 = False
    | row == r2 && col == c2 && num > board !! r1 !! c1 && board !! r1 !! c1 /= 0 = False
    | otherwise = True

is_valid :: Board -> Int -> Int -> Int -> [Comparison] -> Bool
is_valid board row col num comparisons =
    let results = parMap rseq (checkConstraint board row col num) comparisons
    in all id results &&
       notElem num (board !! row) &&
       notElem num (map (!! col) board) &&
       notElem num (concatMap (take 3 . drop (col `div` 3 * 3)) (take 3 $ drop (row `div` 3 * 3) board))

solve :: Int -> Int -> Board -> [Comparison] -> Maybe Board
solve row col board comparisons
    | row == 9 = Just board
    | col == 9 = solve (row + 1) 0 board comparisons
    | otherwise =
        let validNums = filter (\num -> is_valid board row col num comparisons) [1..9]
        in  foldr (\num acc -> case acc of
                Just _ -> acc
                Nothing -> solve row (col + 1) (updateBoard row col num board) comparisons
            ) Nothing validNums

main :: IO ()
main = do
    let emptyBoard = replicate 9 (replicate 9 0)
    let comparisons = [(1, 0, 0, 0),(0, 1, 0, 0),(0, 1, 0, 2),(0, 1, 1, 1),(0, 2, 1, 2),(0, 3, 0, 4),(1, 3, 0, 3),(1, 4, 0, 4),(1, 5, 0, 5),(0, 5, 0, 4),(0, 6, 0, 7),(0, 6, 1, 6),(0, 7, 0, 8),(1, 7, 0, 7),(1, 0, 1, 1),(1, 0, 2, 0),(1, 1, 1, 2),(2, 1, 2, 0),(2, 1, 1, 1),(2, 2, 2, 1),(2, 2, 1, 2),(1, 3, 2, 3),(1, 4, 1, 3),(1, 4, 2, 4),(2, 3, 2, 4),(2, 5, 2, 4),(1, 6, 1, 7),(2, 6, 1, 6),(2, 6, 2, 7),(2, 7, 1, 7),(2, 7, 2, 8),(2, 8, 1, 8),(1, 8, 1, 7),(1, 8, 0, 8),(3, 0, 3, 1),(3, 0, 4, 0),(3, 1, 3, 2),(4, 0, 4, 1),(4, 0, 5, 0),(4, 1, 3, 1),(4, 1, 4, 2),(4, 1, 5, 1),(5, 0, 5, 1),(4, 2, 3, 2),(5, 2, 5, 1),(5, 2, 4, 2),(3, 4, 3, 3),(3, 3, 4, 3),(3, 4, 3, 5),(3, 5, 4, 5),(4, 4, 3, 4),(4, 4, 3, 4),(4, 4, 3, 4),(4, 4, 4, 5),(4, 4, 4, 3),(4, 4, 5, 4),(5, 3, 4, 3),(5, 3, 5, 4),(5, 5, 5, 4),(5, 5, 4, 5),(3, 7, 3, 6),(3, 7, 4, 7),(3, 8, 3, 7),(3, 8, 4, 8),(4, 6, 3, 6),(4, 7, 4, 6),(4, 7, 5, 7),(4, 8, 4, 7),(5, 6, 4, 6),(5, 7, 5, 6),(5, 8, 7, 7),(5, 8, 4, 8),(6, 1, 6, 0),(6, 1, 6, 2),(6, 1, 7, 1),(6, 0, 7, 0),(6, 2, 7, 2),(7, 1, 7, 0),(7, 1, 7, 2),(7, 1, 8, 1),(8, 0, 7, 0),(8, 0, 8, 1),(8, 1, 8, 2),(7, 2, 8, 2),(6, 4, 6, 3),(6, 5, 6, 4),(7, 3, 6, 3),(7, 3, 7, 4),(7, 4, 6, 4),(7, 4, 7, 5),(6, 5, 7, 5),(8, 3, 7, 3),(8, 3, 8, 4),(8, 4, 7, 4),(8, 4, 8, 5),(7, 5, 8, 5),(6, 6, 6, 7),(6, 6, 7, 6),(6, 8, 6, 7),(6, 8, 7, 8),(7, 7, 6, 7),(7, 7, 7, 8),(7, 7, 7, 6),(7, 7, 8, 7),(8, 6, 7, 6),(8, 7, 8, 6),(8, 7, 8, 8),(7, 8, 8, 8)]
    case solve 0 0 emptyBoard comparisons of
        Just solution -> putStrLn $ unlines $ map (unwords . map show) solution
        Nothing -> putStrLn "No solution exists."