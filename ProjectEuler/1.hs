main = do
    let total = sum . (filter isMultiple) $ [1..999]
    print total

isMultiple :: Int -> Bool
isMultiple x = (x `mod` 3 == 0) || (x `mod` 5 == 0)
