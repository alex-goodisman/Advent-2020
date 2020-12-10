import Data.IntSet (empty, insert, member)
import Data.Maybe (isJust, fromJust)
import Data.List (find, partition)

twosum :: Int -> [Int] -> Maybe Int
twosum target list =
    let half = target `div` 2 in
    if even target && length (filter (==half) list) >= 2 then Nothing else
        let set = foldr insert empty list in
        if isJust . find (\v -> not (even target && v == half) && member (target - v) set) $ list then Nothing else Just target

takeUntilSum :: [Int] -> Int -> Int -> ([Int], Int, Int)
takeUntilSum (h:t) s i | s > 0 = takeUntilSum t (s - h) (i + 1)
takeUntilSum l s i = (l,s,i)

step :: Int -> (Bool, Int, [Int], Int) -> Int -> (Bool, Int, [Int], Int)
step target (True, total, buffer, size) _ = (True, total, buffer, size)
step target (False, total, buffer, size) next =
    let iter = total + next in
    if iter < target then (False, iter, buffer, size + 1) else
        let (rem, lost, sizedec) = takeUntilSum buffer (iter - target) 0 in (lost == 0, target + lost, rem, size + 1 - sizedec)

main :: IO ()
main = do
    content <- readFile "input.txt"
    let count = 25
    let nums = read <$> lines content :: [Int]
    let tests = drop count nums
    let target = fromJust . fst . foldl (\(done,ns) t -> (if isJust done then done else twosum t (take count ns), tail ns)) (Nothing, nums) $ tests
    putStrLn . show $ target
    let (_, _, buf, size) = foldl (step target) (False, 0, nums, 0) nums
    let list = take size buf
    putStrLn . show $ (minimum list + maximum list)
