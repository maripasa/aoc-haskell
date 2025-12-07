import Data.List
import Data.Array
import Debug.Trace

removeDuplicates :: Ord a => [a] -> [a]
removeDuplicates = rem' . sort
  where
    rem' (a:b:cs)
      | a == b = rem' (b:cs)
      | otherwise = a : rem' (b:cs)
    rem' a = a

convertMatrix mat = array ((0,0), (ly - 1, lx - 1)) . concat
  . map (\(y, line) -> map (\(x, item) -> ((y,x), item)) . zip [0..] $ line) . zip [0..] $ mat
  where
    ly = length mat
    lx = length (mat !! 1)

findArrIdx target arr = lookup target . map flipPair . assocs $ arr
findIdx target = lookup target . map flipPair . zip [0..]

flipPair (a, b) = (b, a)

simulate :: [String] -> Int
simulate manifold = length . solve [maybe 0 id (findIdx 'S' (head manifold))] $ manifold
  where
    solve :: [Int] -> [String] -> [Int]
    solve beams [] = beams
    solve beams (m:ms) = (\beams' -> solve (concat beams') ms) . map (logic m) $ beams
    logic m n
      | m !! n == '^' = [n-1, n+1]
      | otherwise = [n]

main= readFile "input" >>= \file -> return . simulate . lines $ file
