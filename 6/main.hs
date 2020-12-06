import Data.List.Split (splitOn)
import Data.HashSet (fromList, delete, size, intersection)

main :: IO ()
main = do
    content <- readFile "input.txt"
    let blocks = splitOn "\n\n" content
    let answers = (size . delete '\n' . fromList) <$> blocks
    putStrLn . show . sum $ answers
    let answers2 = (size . foldr1 intersection . (<$>) fromList . lines) <$> blocks
    putStrLn . show . sum $ answers2
