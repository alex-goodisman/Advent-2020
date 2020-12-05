import Data.List.Split (splitOn, splitOneOf)
import Data.HashSet (HashSet, delete, fromList, null)
import Data.HashMap.Lazy (HashMap, fromList, member, (!))
import Data.Char (isAlpha, isNumber)
import Data.Tuple (swap)

parse :: String -> HashMap String String
parse s =
    let grps = splitOneOf " \n" s in
    let codes = filter (not . Prelude.null . fst) $ break (==':') <$> grps in
    Data.HashMap.Lazy.fromList $ ((<$>) tail) <$> codes

tests :: [(String, String -> Bool)]
tests = [("byr", (\v -> v >= 1920 && v <= 2002) . read ),
    ("iyr", (\v -> v >= 2010 && v <= 2020) . read),
    ("eyr", (\v -> v >= 2020 && v <= 2030) . read),
    ("hgt", (\(str,n) -> (str == "cm" && n >= 150 && n <= 193) || (str == "in" && n >= 59 && n <= 76)) . (<$>) read . swap . break isAlpha),
    ("hcl", (\(pre,post) -> length pre == 1 && length post == 6 && all (\p -> isNumber p || (p >= 'a' && p <= 'f')) post) . span (=='#')),
    ("ecl", flip elem ["amb","blu","brn","gry","grn","hzl","oth"]),
    ("pid", \p -> all isNumber p && length p == 9)]

validKeys :: HashMap String String -> Bool
validKeys m =
    foldr (\(k,_) b -> b && member k m) True tests

valid :: HashMap String String -> Bool
valid m =
    foldr (\(k,p) b -> b && member k m && p (m ! k)) True tests

main :: IO ()
main = do
    content <- readFile "input.txt"
    let blocks = parse <$> splitOn "\n\n" content
    putStrLn . show . length . filter validKeys $ blocks
    let goods = filter valid $ blocks
    putStrLn . show . length . filter valid $ blocks
