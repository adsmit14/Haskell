-- 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
-- What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

import Data.List

main = print $ find isValid [20,40..]

isValid :: Int -> Bool
isValid x = all (\y -> (x `mod` y) == 0) ys
    where ys = [20,19,18,17,16,15,14,13,12,11]
