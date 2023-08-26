mdc :: Int -> Int -> Int
mdc x y = 
    if y == 0 then
        x
    else
        mdc y (x `mod` y)

coprimos :: Int -> Int -> Int
coprimos x y = 
    if mdc x y == 1 then
        1
    else 
        0

totiente :: Int -> Int -> Int
totiente _ 0 = 0
totiente x n = coprimos x n + totiente x (n-1)

main :: IO ()
main = do
    putStrLn "Digite um número n:"
    input <- getLine
    let n = read input :: Int
    let result = totiente n n
    print result