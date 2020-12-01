calculateFuel :: Integer -> Integer
calculateFuel n
    | n <= 0 = 0
    | otherwise =
        let fuelMass = max 0 ((div n 3) - 2)
        in  fuelMass + calculateFuel fuelMass

main = do
    contents <- getContents
    let weights = map read (lines contents)
    putStrLn (show $ sum $ map calculateFuel weights)
