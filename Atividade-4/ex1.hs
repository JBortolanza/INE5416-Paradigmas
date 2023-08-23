elevate :: Float -> Float -> Float
elevate x y = x ** y

main = do
    input <- getLine
    let x = read input :: Float
    input <- getLine
    let y = read input :: Float
    print (elevate x y)