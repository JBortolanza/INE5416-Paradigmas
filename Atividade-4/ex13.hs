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
    print (x * (y `div` (mdc x y))) -- Calculo de MMC utilizando MDC