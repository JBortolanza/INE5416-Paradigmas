avg :: Float -> Float -> Float -> String
avg x y z = 
    if ((x+y+z)/3) >= 6 then
        "Aprovado"
    else
        "Reprovado"

main = do
    input <- getLine
    let x = read input :: Float
    input <- getLine
    let y = read input :: Float
    input <- getLine
    let z = read input :: Float
    print (avg x y z)