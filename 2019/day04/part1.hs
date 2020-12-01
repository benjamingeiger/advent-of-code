pairs :: [a] -> [(a, a)]
pairs (x1:x2:xs) = (x1, x2) : (pairs (x2:xs))
pairs _ = []

digits :: Int -> [Int]
digits 0 = []
digits n =
    let (q, r) = divMod n 10
    in  (digits q) ++ [r]

criterion2 :: [Int] -> Bool
criterion2 digits = foldl (||) False $ map (\(x, y) -> x == y) $ pairs digits

criterion3 :: [Int] -> Bool
criterion3 digits = foldl (&&) True $ map (\(x, y) -> x <= y) $ pairs digits

evaluate :: Int -> Bool
evaluate n =
    let ds = digits n
    in (criterion2 ds) && (criterion3 ds)

main :: IO ()
main = putStrLn $ show $ length $ filter evaluate [234208..765869]
    
