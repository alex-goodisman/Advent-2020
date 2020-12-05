parse :: String -> (Int, Int, Char, String)
parse s =
    let (s1, _:_:pass) = break (==':') s in
    let (s2, _:char:[]) = break (==' ') s1 in
    let (lo, _:hi) = break (=='-') s2 in
    (read lo,read hi,char,pass)

valid :: (Int, Int, Char, String) -> Bool
valid (lo, hi, char, pass) =
    let count = length . filter (==char) $ pass in
    count >= lo && count <= hi

valid2 :: (Int, Int, Char, String) -> Bool
valid2 (lo, hi, char, pass) =
    (pass !! (lo - 1) == char) /= (pass !! (hi - 1) == char)

main :: IO ()
main = do
    content <- readFile "input.txt"
    let values = parse <$> lines content
    putStrLn . show . length . filter valid $ values
    putStrLn . show . length . filter valid2 $ values
