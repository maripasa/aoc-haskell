import Split
import Data.List 
import Data.Ord
import String

part1 :: [[Int]] -> Int
part1 [] = 0
part1 ([l,w,h]:xs) = area + part1 xs
  where
    area = (+smallest) . sum . map (2*) $ [a,b,c] 
    a = l*w
    b = w*h
    c = h*l
    smallest = minimum [a, b, c]


part2 :: [[Int]] -> Int
part2 [] = 0
part2 ([l,w,h]:xs) = lribbon + bow + part2 xs
  where
    lribbon = (\[x,y] -> 2*(x+y)) . take 2 . sort $ [l,w,h]
    bow = l*w*h
   
parse = map (map stringToInt . splitOn 'x') . words


main :: IO ()
main = readFile "day02/input" >>= \file -> do
  let content = parse file
  (print . part1) content
  (print . part2) content
