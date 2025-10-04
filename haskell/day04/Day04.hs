

part1 = id


part2 = id


parse = id


main :: IO ()
main = readFile "day04/input" >>= \file -> do
  let content = parse file
  (print . part1) content
  (print . part2) content
