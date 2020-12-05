import Data.List (sort, find)
import Data.Maybe (fromJust)

parseBin :: String -> Int
parseBin [] = 0
parseBin (h:t) = 2 * parseBin t + fromEnum (h == 'B' || h == 'R')

main :: IO ()
main = do
    content <- readFile "input.txt"
    let values = lines content
    let ids = reverse . sort $ parseBin . reverse <$> values
    putStrLn . show . head $ ids
    putStrLn . show . subtract 1 . fst . fromJust . find (\(a,b) -> b < a - 1) $ zip ids (tail ids)
