import Data.HashSet (empty, insert, member)
import Data.List (find)
import Data.Maybe (fromMaybe, isJust)
import Control.Monad (join)

twosum :: Int -> [Int] -> Maybe Int
twosum target list =
    let set = foldr insert empty list in
    let ret = find (\v -> member (target - v) set) list in
    (\r -> r * (target - r)) <$> ret

threesum :: Int -> [Int] -> Maybe Int
threesum target list =  join . find isJust .
    map  (\v -> (*v) <$> twosum (target - v) list) $ list

main :: IO ()
main = do
    content <- readFile "input.txt"
    let values = read <$> (lines content) :: [Int]
    putStrLn $
        fromMaybe "Not found" (show <$> twosum 2020 values)
    putStrLn $
        fromMaybe "Not found" (show <$> threesum 2020 values)
