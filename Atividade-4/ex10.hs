bigger :: Float -> Float -> Float -> Float
bigger x y z | x>y && x>z = x
             | y>x && y>z = y
             | otherwise = z

main = do
    input <- getLine
    let x = read input :: Float
    input <- getLine
    let y = read input :: Float
    input <- getLine
    let z = read input :: Float
    print (bigger x y z)