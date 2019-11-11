main = do
    let r = sum . (filter even) . takeWhile (< 4000000) $ fibs
    print r

fibs :: [Integer]
fibs = map fst $ iterate (\(a, b) -> (b, a + b)) (0,1)
