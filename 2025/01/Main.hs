import Debug.Trace (traceShowId)

parse :: String -> [Either Int Int]
parse input = map parseOne (words input)
  where
    parseOne ('L':num) = Left (read num)
    parseOne ('R':num) = Right (read num)
    parseOne _ = error "what"

part1 :: [Either Int Int] -> Int
part1 rotations = rotate rotations 50 0
  where
    rotate (r:rs) state zeros =
      let s' = flipDial r state
      in rotate rs s' (zeros + fromEnum (s' == 0))
    rotate [] _ zeros = zeros

flipDial :: Either Int Int -> Int -> Int
flipDial (Right amount) start = (start + amount) `mod` 100
flipDial (Left amount) start = (start - amount) `mod` 100

flipDialCount :: Either Int Int -> Int -> (Int, Int)
flipDialCount (Right amount) start =
    let end   = (start + amount) `mod` 100
        wraps = (start + amount) `div` 100
    in (end, wraps)

flipDialCount (Left amount) start =
    let (a, b) = (start - amount) `divMod` 100
        base   = abs a
        extra
          | start == 0 && b > 0 = -1
          | start > 0  && b == 0 = 1
          | otherwise            = 0
    in (b, base + extra)

part2 :: [Either Int Int] -> Int
part2 rotations = rotate rotations 50 0
  where
    rotate (r:rs) state zeros =
      let (s', skips) = flipDialCount r state
      in rotate rs s' (zeros + skips)
    rotate [] _ zeros = zeros


main :: IO ()
main =
  readFile "input" >>= \file -> do
    let content = parse file
    print content
    (print . part1) content
    (print . part2) content
