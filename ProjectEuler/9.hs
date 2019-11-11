import Data.List

s = 1000

main = do
    let xs = [(a, b, (s - a - b)) | a <- [1..(s `div` 3)], b <- [1..(s `div` 2)]]
    let r = find isValid xs
    case r of
        (Just (a,b,c)) -> print (a * b * c)
        _              -> print "Not Found"

isValid (a, b, c)
    | a ^ 2 + b ^ 2 == c ^ 2 = True
    | otherwise              = False
