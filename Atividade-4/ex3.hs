area :: Float -> Float -> Float
area x y = (x * y)/2

main = do
    input <- getLine
    let x = read input :: Float
    input <- getLine
    let y = read input :: Float
    print (area x y)