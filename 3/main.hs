collisions :: Int -> [[Bool]] -> Int
collisions offset rows =  snd . foldl
    (\(pos, count) row -> ((pos + offset) `mod` length row, count + fromEnum (row !! pos))) (0, 0) $ rows

skips :: Int -> [a] -> [a]
skips n [] = []
skips n list = head list : skips n (drop n list)

slope :: Int -> Int -> [[Bool]] -> Int
slope right down = collisions right . skips down

main :: IO ()
main = do
    content <- readFile "input.txt"
    let rows = ((<$>) (=='#')) <$> lines content
    putStrLn . show . slope 3 1 $ rows
    let slopes = (flip ($) rows) <$> [slope 1 1, slope 3 1, slope 5 1, slope 7 1, slope 1 2]
    putStrLn . show . product $ slopes
