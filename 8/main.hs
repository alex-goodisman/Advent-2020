import Data.Char (isSpace)
import Data.IntMap.Lazy as Map (IntMap, empty, insert, alter, findWithDefault, lookup, delete)
import Data.Maybe (fromMaybe)
import Data.IntSet as Set (IntSet, empty, insert, member)

parse :: String -> (String, Int)
parse = (<$>) (read . dropWhile (\s -> isSpace s || s == '+')) .  break isSpace

collect :: IntMap [Int] -> Int -> IntSet -> IntSet
collect m i s =
    let children = findWithDefault [] i m in
    let s2 = foldr Set.insert s children in
    foldr (collect m) s2 children

walk :: IntMap (String, Int) -> IntSet -> Int -> Int -> Int
walk m reachable acc i =
    let m2 = delete i m in
    case Map.lookup i m of
        Just ("acc", amt) -> walk m2 reachable (acc + amt) (i + 1)
        Just ("nop", off) | member (i + off) reachable -> walk m2 Set.empty acc (i + off)
        Just ("nop", _) -> walk m2 reachable acc (i + 1)
        Just ("jmp", _) | member (i + 1) reachable -> walk m2 Set.empty acc (i + 1)
        Just ("jmp", off) -> walk m2 reachable acc (i + off)
        _ -> acc

main :: IO ()
main = do
    content <- readFile "input.txt"
    let (count, dataMap, backMap) = foldl (\(ind, m, bm) (str,off) -> (ind + 1, Map.insert ind (str,off) m,
            alter (Just . (ind:) . fromMaybe []) (ind + if str == "jmp" then off else 1) bm)) (0, Map.empty, Map.empty) $ parse <$> lines content
    putStrLn . show $ walk dataMap Set.empty 0 0
    let canReach = collect backMap count Set.empty
    putStrLn . show $ walk dataMap canReach 0 0
