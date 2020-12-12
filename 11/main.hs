import Data.HashMap.Lazy (HashMap, empty, insert, lookup, mapWithKey)
import Data.Maybe (fromJust)

neighbors :: HashMap (Int, Int) Char -> (Int, Int) -> Int
neighbors m (i,j) =
    length . filter (==(Just '#')) $ [Data.HashMap.Lazy.lookup (y,x) m | y <- [i-1..i+1], x <- [j-1..j+1], y /= i || j /= x]

rule :: Char -> Int -> Int -> Char
rule 'L' 0 _ = '#'
rule '#' n m| n >= m = 'L'
rule c _ _ = c


steps :: Eq a => (a -> a) -> a -> a
steps f a =
     let a' = f a in
     if a == a' then a' else steps f a'

view :: Int -> (Int, Int) -> (Int, Int) -> HashMap (Int, Int) Char -> Maybe Char
view n (y,x) (i,j) m =
    let v = Data.HashMap.Lazy.lookup (i+n*y, j+n*x) m in
    case v of
        Just '.' -> view (n+1) (y,x) (i,j) m
        _ -> v

viewed :: HashMap (Int, Int) Char -> (Int, Int) -> Int
viewed m (i,j) =
    length . filter (==(Just '#')) $ [view 1 (y,x) (i,j) m | y <- [-1..1], x <- [-1..1], y /= 0 || x /= 0]

main :: IO ()
main = do
    content <- readFile "input.txt"
    let rows = lines content
    let grid = zip [0..] $ zip [0..] <$> rows :: [(Int, [(Int, Char)])]
    let m = foldr (\(i, rs) m -> foldr (\(j, r) m' -> insert (i,j) r m') m rs) empty grid
    putStrLn . show . sum $ (fromEnum . (=='#')) <$> steps (\d -> mapWithKey (\(i,j) c -> rule c (neighbors d (i,j)) 4) d) m
    putStrLn . show . sum $ (fromEnum . (=='#')) <$> steps (\d -> mapWithKey (\(i,j) c -> rule c (viewed d (i,j)) 5) d) m
