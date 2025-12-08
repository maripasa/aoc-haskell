import Data.List
import qualified Data.Set as Set

distance :: [Int] -> [Int] -> Int
distance a b = sum . map (^2) . zipWith (-) a $ b

ssplitAt del str = splitAt [] str
  where
  splitAt acc (c:cs)
    | c == del  = reverse acc : splitAt [] cs
    | otherwise = splitAt (c:acc) cs
  splitAt acc [] = [reverse acc]

tidy lst = rem' lst
  where
    rem' [] = []
    rem' ((x,y,_):b@(x',y',dis):cs)
      | (y,x) == (x', y') = rem' (b:cs)
      | otherwise = (x,y) : rem' (b:cs)
    rem' [(x,y,_)] = [(x,y)]

distances :: [[Int]] -> [([Int], [Int])]
distances points = tidy . sortBy (\(_a,_b,dis) (_x,_y,dis') -> compare dis dis') $ [(x,y, distance x y) | x <- points, y <- points, x /= y]

unbox = either id id

connect ((x,y):ds) = con ds [Set.fromList [x,y]]
  where
    con [] sets = sets
    con ((x,y):ds) sets = con ds (findAdd x y sets [])

    findAdd x y [] acc = ((Set.fromList [x,y]):acc)

    findAdd x y sets@(s:ss) acc
      | x `Set.member` s =

        if y `Set.member` s
        then acc ++ sets
        else (addNext y s ss []) ++ acc

      | y `Set.member` s =

        if x `Set.member` s
        then acc ++ sets
        else (addNext x s ss []) ++ acc

      | otherwise =
        
        findAdd x y ss (s:acc)

      where
      -- We found x, y isn't in the same set, so let's find y and union to x's set
        addNext v fs (n:ns) acc
          | v `Set.member` n = ((Set.union fs n):acc)
          | otherwise = addNext v fs ns (n:acc)
        addNext v fs [] acc = ((Set.insert v fs):acc)

main = readFile "input" >>= \file -> return
  . product
  . take 3
  . reverse
  . sort
  . map Set.size
  . connect
  . take 1000
  . distances
  . map (map read . ssplitAt ',') . lines $ file
