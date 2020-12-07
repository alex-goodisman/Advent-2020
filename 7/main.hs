import Data.List.Split (splitOn)
import Data.Char (isNumber, isSpace)
import Data.HashMap.Lazy (HashMap, empty, insert, lookupDefault)
import Data.HashSet (HashSet, empty, insert, size, member)
import Data.List (isPrefixOf)

parse :: String -> (String, [(Int, String)])
parse input =
    let [val, text] = splitOn " bags contain " input in
    let pairs = span isNumber <$> (filter (not . isPrefixOf "no other") .  splitOn ", " $ text) in
    let keys =  (\(n,s) -> (read n, head . splitOn " bag" . dropWhile isSpace $ s)) <$> pairs in
    (val, keys)


bfs :: HashMap String [String] -> String -> HashSet String -> HashSet String
bfs hm key found =
    let children = filter (not . (\a -> member a found)) $ lookupDefault [] key hm in
    let set = foldr Data.HashSet.insert found children in
    foldr (bfs hm) set children

dfs :: HashMap String [(Int, String)] -> String -> Int
dfs hm key =
    let children = lookupDefault [] key hm in
    let counts = (\(n,s) -> n * (1 + dfs hm s) ) <$> children in
    sum counts

main :: IO ()
main = do
    content <- readFile "input.txt"
    let infos = parse <$> lines content
    let hm = foldr (\(v, ks) hm -> foldr (\(_,k) hm' -> Data.HashMap.Lazy.insert k (v:(lookupDefault [] k hm')) hm') hm ks) Data.HashMap.Lazy.empty $ infos
    putStrLn . show . size $ bfs hm "shiny gold" Data.HashSet.empty
    let hm2 = foldr (\(k, vs) hm -> Data.HashMap.Lazy.insert k vs hm) Data.HashMap.Lazy.empty infos
    putStrLn . show $ dfs hm2 "shiny gold"
