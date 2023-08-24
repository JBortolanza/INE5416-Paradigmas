bhaskara :: Float -> Float -> Float -> String
bhaskara a b c = 
    if (b**2-4*a*c) < 0 then
        "Equacao invalida"
    else
        

main = do
    input <- getLine
    let x = read input :: Float
    input <- getLine
    let y = read input :: Float
    input <- getLine
    let z = read input :: Float
    print (bhaskara x y z)