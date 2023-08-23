fxor :: Bool -> Bool -> Bool
fxor False False = False
fxor False True = True
fxor True False = True
fxor True True = False

main = do
    putStrLn "Digite o valor de x (True/False):"
    input <- getLine
    let x = read input :: Bool
    putStrLn "Digite o valor de y (True/False):"
    input <- getLine
    let y = read input :: Bool
    print (fxor x y)