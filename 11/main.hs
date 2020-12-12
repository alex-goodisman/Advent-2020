import Data.Array (listArray, (!), Array, bounds, array)

neighbors :: Array Int (Array Int Char) -> Int -> Int -> Int
neighbors arr i j =
    let (miny, maxy) = bounds arr in
    let (minx, maxx) = bounds (arr ! miny) in
    let ns = [arr ! y ! x | y <- [i-1..i+1], x <- [j-1..j+1], miny <= y && maxy >= y, minx <= x && maxx >= x, y /= i || x /= j ] in
    length . filter (=='#') $ ns

rule :: Int -> Char -> Char
rule 0 'L' = '#'
rule n '#' | n >= 4 = 'L'
rule _ c = c

step :: Array Int (Array Int Char) -> Array Int (Array Int Char)
step arr =
    let (miny, maxy) = bounds arr in
    let (minx, maxx) = bounds (arr ! miny) in
    listArray (miny, maxy) ((\i -> listArray (minx, maxx) ((\j -> rule (neighbors arr i j) (arr ! i ! j)) <$> [minx..maxx])) <$> [miny..maxy])

steps :: Array Int (Array Int Char) -> Array Int (Array Int Char)
steps arr =
    let narr = step arr in
    if arr == narr then narr else steps narr

main :: IO ()
main = do
    content <- readFile "input.txt"
    let rows = lines content
    let width = length (head rows)
    let height = length rows
    let arrs = listArray (0, width - 1) <$> rows
    let arr = listArray (0, height - 1) arrs
    putStrLn . show . sum $ (sum . (<$>) (fromEnum . (=='#'))) <$> steps arr
