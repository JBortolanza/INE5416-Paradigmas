primo :: Int -> Int -> String
primo n 1 = "Primo"
primo n x =
    if divisivel n x then
        "Nao primo"
    else
        primo n (x-1)

divisivel :: Int -> Int -> Bool
divisivel x y = 
    if x `mod` y == 0 then
        True
    else
        False


main = do
    input <- getLine
    let n = read input :: Int
    print (primo n (n-1))