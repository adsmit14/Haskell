range = reverse [100..999]
main = do
    let products = [ x * y | x <- range, y <- range]
    let answer = maximum . filter isPalindrome $ products
    print answer

isPalindrome :: Int -> Bool
isPalindrome x = reverse xs == xs
    where xs = show x
