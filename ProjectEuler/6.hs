main = print (sqSums - sumSqs)
    where sqSums = sum [1..100] ^ 2
          sumSqs = sum . map (^2) $ [1..100]
