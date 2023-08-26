operador :: String -> Int -> Int -> Int
operador "+" x y = x + y
operador "-" x y = x - y
operador "/" x y = x `div` y
operador "*" x y = x * y

main = do
    input <- getLine
    let x = read input :: Int
    input <- getLine
    let y = read input :: Int
    putStrLn "Digite um operador entre aspas"
    input <- getLine
    let op = read input :: String
    print (operador op x y)