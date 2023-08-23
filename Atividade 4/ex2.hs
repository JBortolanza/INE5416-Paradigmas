absolute :: Int -> Int
absolute x = 
    if x >= 0 then
        x
    else
        -x

main = do
    input <- getLine
    let a = read input :: Int
    print (absolute a)