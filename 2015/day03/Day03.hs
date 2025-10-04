import Data.List

up (x, y) = (x, y + 1)
down (x, y) = (x, y - 1)
left (x, y) = (x - 1, y)
right (x, y) = (x + 1, y)

part1 :: String -> Int
part1 = length . removeDuplicates . foldr go [(0, 0)]
  where
    go '^' vis = (up . head $ vis) : vis
    go 'v' vis = (down . head $ vis) : vis
    go '<' vis = (left . head $ vis) : vis
    go '>' vis = (right . head $ vis) : vis
    go _ vis = vis

part2 = length . removeDuplicates . folde [(0,0),(0,0)]
  where
    folde vis [] = vis
    folde (santa':visi) [santa] = go santa santa' : santa' : visi
    folde (santa':robot':visi) (santa:robot:inst)
      = folde (go santa santa' : go robot robot' : santa' : robot' : visi) inst
    go '^' = up
    go 'v' = down
    go '<' = left
    go '>' = right
    go _ = id


parse = id

-- nub works but is n^2
removeDuplicates :: (Ord a) => [a] -> [a]
removeDuplicates = map head . group . sort

main :: IO ()
main =
  readFile "day03/input" >>= \file -> do
    let content = parse file
    (print . part1) content
    (print . part2) content
