calculateFuel :: Integer -> Integer
calculateFuel n = (div n 3) - 2

main = do
    contents <- getContents
    let weights = map read (lines contents)
    putStrLn (show $ sum $ map calculateFuel weights)
