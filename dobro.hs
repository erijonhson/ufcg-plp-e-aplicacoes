doubleMe :: Int -> Int
doubleMe x = x * 2

main = do  
    input <- getLine
    let num = doubleMe (read input)
    print num
