import System.Environment
import Data.List

main = do
    (f1:f2:_) <- getArgs
    c1 <- readFile f1
    c2 <- readFile f2
    putStr $ show $ (lines c1) \\ (lines c2)