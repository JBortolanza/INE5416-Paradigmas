mdc :: Int -> Int -> Int
mdc x y = 
    if y==0 then
        x
    else
        mdc y (x `mod` y)


main = do
    input <- getLine
    let x = read input :: Int
    input <- getLine
    let y = read input :: Int
    input <- getLine
    let z = read input :: Int
    print (mdc (mdc x y) z)