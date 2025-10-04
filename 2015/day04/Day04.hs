import Md5


part1 :: Int -> String -> Int
part1 num input
  | take 6 hash == "000000" = num
  | otherwise = part1 (num + 1) input
  where
    hash = md5s (Str (input ++ show num))
       




test x = md5s (Str x)

part2 = id


parse = head . words


main :: IO ()
main = readFile "day04/input" >>= \file -> do
  let content = parse file
      result1 = part1 0 content
  print result1
  print (test (content ++ show result1))
  (print . part2) content
