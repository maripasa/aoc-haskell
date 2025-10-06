import Split
import Data.List

part1 :: [String] -> Int
part1 = length . filter (\x -> threeVowels x && twoInRow x && forbiddenElems x)
  where
    threeVowels = (\x -> length x >= 3) . filter (`elem` "aeiou")

    twoInRow (x : y : cs) = x == y || twoInRow (y : cs)
    twoInRow _ = False

    forbiddenElems (x : y : cs) = [x, y] `notElem` ["ab", "cd", "pq", "xy"] && forbiddenElems (y : cs)
    forbiddenElems _ = True

part2 :: [String] -> Int
part2 = length . filter (\x -> inBetween x && noOverlap x && hasRepeated (pairs x))
  where
    inBetween (x:y:z:cs) = x == z || inBetween (y:z:cs)
    inBetween _ = False
    
    noOverlap (x:y:z:cs) = not (x == y && y == z) && noOverlap (y:z:cs)
    noOverlap _ = True

    pairs :: (Eq a) => [a] -> [[a]]
    pairs = pairs' []
      where
        pairs' lpair (x:y:cs) = pairs' ([x,y]:lpair) (y:cs)
        pairs' lpair _ = lpair

    hasRepeated :: (Eq a) => [a] -> Bool
    hasRepeated x = length x /= length (nub x)

      

parse = words

main :: IO ()
main =
  readFile "day05/input" >>= \file -> do
    let content = parse file
    (print . part1) content
    (print . part2) content
