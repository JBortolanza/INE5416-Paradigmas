mdc :: Int -> Int -> Int
mdc x y = 
    if y==0 then
        x
    else
        mdc y (x `mod` y)

coprimos :: Int -> Int -> String
coprimos x y = 
    if mdc x y == 1 then
        "Coprimos"
    else 
        "Nao coprimos"

main = do
    input <- getLine
    let x = read input :: Int
    input <- getLine
    let y = read input :: Int
    print (coprimos x y)