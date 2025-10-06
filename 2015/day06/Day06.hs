import Data.Bits
import Data.Word
import Data.List (foldl')
import Debug.Trace
import Split

data Operation = TurnOn | TurnOff | Toggle
  deriving (Show)

data Rectangle = Rectangle (Int, Int) (Int, Int)
  deriving (Show)

startMatrix :: [[Int]]
startMatrix = replicate 1000 (replicate 1000 0)

part1 :: [(Operation, Rectangle)] -> Int
part1 = sum . concat . foldl' (flip focusApply) startMatrix

focusApply :: (Operation, Rectangle) -> [[Int]] -> [[Int]]
focusApply (op, Rectangle (x1, y1) (x2, y2)) = zipWith updateRow [0..]
  where
    updateRow y row
      | y < y1 = row
      | y > y2 = row
      | otherwise = applyLine op x1 x2 row

applyLine :: Operation -> Int -> Int -> [Int] -> [Int]
applyLine op x1 x2 row =
  let (prefix, rest) = splitAt x1 row
      (target, suffix) = splitAt (x2 - x1 + 1) rest
      apply = case op of
        TurnOn  -> const 1
        TurnOff -> const 0
        Toggle  -> (1 -)
  in prefix ++ map apply target ++ suffix

applyLine2 :: Operation -> Int -> Int -> [Int] -> [Int]
applyLine2 op x1 x2 row =
  let (prefix, rest) = splitAt x1 row
      (target, suffix) = splitAt (x2 - x1 + 1) rest
      apply = case op of
        TurnOn  -> (+ 1)
        TurnOff -> (\x -> if x == 0 then 0 else x - 1)
        Toggle  -> (+ 2)
  in prefix ++ map apply target ++ suffix

foldrTrace :: (Show a) => (a -> b -> b) -> b -> [a] -> b
foldrTrace f z [] = z
foldrTrace f z (x:xs) =
  let res = foldrTrace f z xs
  in trace ("App to: " ++ show x) (f x res)

focusApply2 :: (Operation, Rectangle) -> [[Int]] -> [[Int]]
focusApply2 (op, Rectangle (x1, y1) (x2, y2)) = zipWith updateRow [0..]
  where
    updateRow y row
      | y < y1 = row
      | y > y2 = row
      | otherwise = applyLine2 op x1 x2 row

lowMask :: Int -> Word64
lowMask n
  | n >= 64 = maxBound
  | otherwise = (1 `shiftL` n) - 1

highMask :: Int -> Word64
highMask n
  | n <= 0 = maxBound
  | n >= 64 = 0
  | otherwise = complement (lowMask n)

part2 :: [(Operation, Rectangle)] -> Int
part2 = sum . concat . foldl' (flip focusApply2) startMatrix

parse :: String -> [(Operation, Rectangle)]
parse = map (convert . words) . lines
  where
    convert ["toggle", start, _through, end]      = (Toggle, parseCoord start end)
    convert ["turn", "on", start, _through, end]  = (TurnOn, parseCoord start end)
    convert ["turn", "off", start, _through, end] = (TurnOff, parseCoord start end)
    convert _ = error "Invalid instruction format"

    parseCoord :: String -> String -> Rectangle
    parseCoord start end = Rectangle (parseCoord' start) (parseCoord' end)

    parseCoord' :: String -> (Int, Int)
    parseCoord' = (\[x, y] -> (read x, read y)) . splitOn ','

main :: IO ()
main =
  readFile "day06/input" >>= \file -> do
    let content = parse file
    (print . part1) content
    (print . part2) content
