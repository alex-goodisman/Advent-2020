import Data.List (sort)

main :: IO ()
main = do
    content <- readFile "input.txt"
    let nums = sort $ read <$> lines content :: [Int]
    let (o,t,_) = foldl (\(ones, threes, prev) n -> let diff = n - prev in (ones + fromEnum (diff == 1), threes + fromEnum (diff == 3), n )) (0, 1, 0) nums
    putStrLn . show $ o * t
    putStrLn . show . head . snd . foldl (\(a1:a2:a3:_, n1:n2:n3:_) a ->
        (a:[a1,a2], (n1 + (n2 * fromEnum (a - a2 <= 3)) + (n3 * fromEnum (a - a3 <= 3))):[n1,n2])) ([0,-4,-4], [1,-4,-4]) $ nums
