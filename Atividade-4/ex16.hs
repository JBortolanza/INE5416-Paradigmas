divisivel :: Int -> Int -> String
divisivel x y = 
    if x `mod` y == 0 then
        "Divisivel"
    else
        "Nao divisivel"


main = do
    input <- getLine
    let x = read input :: Int
    input <- getLine
    let y = read input :: Int
    print (divisivel x y)