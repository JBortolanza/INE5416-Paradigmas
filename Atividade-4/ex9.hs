distance :: Float -> Float -> Float -> Float -> Float -> Float -> Float
distance x y z x2 y2 z2 = sqrt((x2-x)**2+(y2-y)**2+(z2-z)**2)

main = do
    input <- getLine
    let x = read input :: Float
    input <- getLine
    let y = read input :: Float
    input <- getLine
    let z = read input :: Float
    input <- getLine
    let x2 = read input :: Float
    input <- getLine
    let y2 = read input :: Float
    input <- getLine
    let z2 = read input :: Float
    print (distance x y z x2 y2 z2)