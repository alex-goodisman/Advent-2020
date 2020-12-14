-- https://math.stackexchange.com/questions/2218763/how-to-find-lcm-of-two-numbers-when-one-starts-with-an-offset
import Data.List.Split (splitOneOf)

ext_euclid :: Integer -> Integer -> (Integer, Integer, Integer)
ext_euclid a b = _ext_euclid (a, 1, 0) (b, 0, 1) where
    _ext_euclid (r1, s1, t1) (r2, s2, t2) =
        let q = r1 `div` r2 in
        let r = r1 - q * r2 in
        if r == 0 then (r2, s2, t2) else _ext_euclid (r2, s2, t2) (r, s1 - q * s2, t1 - q * t2)

posmod :: Integer -> Integer -> Integer
posmod a b = ((a `mod` b) + b) `mod` b

match :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
match (b, j) (a, i)=
    let (g, s, t) = ext_euclid a b in
    let l = a * b `div` g in
    (l, (-(i + j) `div` g * s * a) `posmod` l + i)

main :: IO ()
main = do
    content <- readFile "input.txt"
    let nums = filter (not . null) $ splitOneOf ",\n" content
    let target:intervals = read <$> (filter (/="x") nums) :: [Integer]
    let (interval, remainder) = foldr1 (\a b -> if snd b < snd a then b else a) $ (\x -> (x, x - target `mod` x)) <$> intervals
    putStrLn . show $ interval * remainder
    let offsets = snd . foldl (\(i,l) t -> (i + 1, if t == "x" then l else  (read t, i):l)) (0,[]) . tail $ nums
    putStrLn . show . snd . foldr1 match $ offsets
