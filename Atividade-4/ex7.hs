fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

main = do
    input <- getLine
    let x = read input :: Int
    print (fibonacci x)

