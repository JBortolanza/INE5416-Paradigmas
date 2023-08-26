mdc :: Int -> Int -> Int
mdc x y = 
    if y==0 then
        x
    else
        mdc y (x `mod` y)

coprimos :: Int -> Int -> Bool
coprimos x y = 
    if mdc x y == 1 then
        True
    else 
        False

totiente :: Int -> Int
totiente 1 = 0
totiente n = totiente 

main = do
    input <- getLine
    let x = read input :: Int
    input <- getLine
    let y = read input :: Int
    print (coprimos x y)