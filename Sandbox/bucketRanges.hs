import Data.List
 
numbers :: [Integer]
numbers = [1500, 1765, 2364, 3345, 3700, 5934, 8313]
 
batches :: [Integer] -> [[Integer]]
batches xs = filter (not . null) 
                . map (\(x, y) -> filter (\a -> a >= x && a < y) xs)
                . emptyBuckets 
                $ xs
 
emptyBuckets :: [Integer] -> [(Integer, Integer)]
emptyBuckets = map (\x -> (x * 1000, (x + 1) * 1000)) 
                . enumFromTo 0 
                . (`div` 1000) 
                . maximum
