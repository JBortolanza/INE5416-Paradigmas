triangleValidator :: Float -> Float -> Float -> String
triangleValidator x y z = 
    if (x+y) > z then
        "Triangulo Valido"
    else
        "Triangulo Nao Valido"

main = do
    input <- getLine
    let x = read input :: Float
    input <- getLine
    let y = read input :: Float
    input <- getLine
    let z = read input :: Float
    print (triangleValidator x y z)