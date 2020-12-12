import Data.Char (isAlpha)

step :: (Float, Float, Float) -> (String, Float) ->  (Float, Float, Float)
step (x, y, h) ("E", n) = (x + n, y, h)
step (x, y, h) ("W", n) = (x - n, y, h)
step (x, y, h) ("S", n) = (x, y - n, h)
step (x, y, h) ("N", n) = (x, y + n, h)
step (x, y, h) ("R", n) = (x, y, h - (n * pi / 180))
step (x, y, h) ("L", n) = (x, y, h + (n * pi / 180))
step (x, y, h) ("F", n) = (x + (n * cos h), y + (n * sin h), h)
step d _ = d

step2 :: (Float, Float, Float, Float) -> (String, Float) -> (Float, Float, Float, Float)
step2 (x, y, wx, wy) ("E", n) = (x, y, wx + n, wy)
step2 (x, y, wx, wy) ("W", n) = (x, y, wx - n, wy)
step2 (x, y, wx, wy) ("S", n) = (x, y, wx, wy - n)
step2 (x, y, wx, wy) ("N", n) = (x, y, wx, wy + n)
step2 (x, y, wx, wy) ("R", n) = let (mag, h) = (sqrt $ wx * wx + wy * wy, atan2 wy wx - (n * pi / 180)) in (x, y, mag * cos h, mag * sin h)
step2 (x, y, wx, wy) ("L", n) = let (mag, h) = (sqrt $ wx * wx + wy * wy, atan2 wy wx + (n * pi / 180)) in (x, y, mag * cos h, mag * sin h)
step2 (x, y, wx, wy) ("F", n) = (x + n * wx, y + n * wy, wx, wy)
step2 d _ = d

main :: IO ()
main = do
    content <- readFile "input.txt"
    let vals = ((<$>) read . span isAlpha) <$> lines content
    let (x, y, _) = foldl step  (0, 0, 0) vals
    putStrLn . show . round $ abs x + abs y
    let (nx, ny, _, _) = foldl step2 (0, 0, 10, 1) vals
    putStrLn . show . round $ abs nx + abs ny
