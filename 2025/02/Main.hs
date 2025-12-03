import Data.List (sort)

ssplitAt del str = splitAt "" str
  where
  splitAt acc (c:cs)
    | c == del  = reverse acc : splitAt "" cs
    | otherwise = splitAt (c:acc) cs
  splitAt acc [] = [reverse acc]

strip = reverse . remWhite . reverse . remWhite
  where remWhite = dropWhile (\x -> elem x " \n")

read2 :: (String, String) -> (Int, Int)
read2 (st, en) = (read st, read en)

lenNum = length . show

getFirst :: Int -> Int
getFirst n = (\(st, en) -> if st >= en then st else st + 1) . read2 . splitAt (div len 2) . show $ n
  where len = lenNum n

-- must be divisor, actually
splitIn n [] = []
splitIn n lst = take n lst : splitIn n (drop n lst)

removeDuplicates :: Ord a => [a] -> [a]
removeDuplicates = rem' . sort
  where
  rem' (a:b:cs)
    | a == b = rem' (b:cs)
    | otherwise = a : rem' (b:cs)
  rem' a = a

-- 123 000 -> 123
-- 473 473 -> 473

-- 123 - 473 = sum . (\x -> x * (10 ** )) . [123 .. 473]
sumEqualLength st en = sum . map (\x -> x * (10 ^ len) + x) $ [st..en]
  where len = lenNum st

getLast :: Int -> Int
getLast n = (\(st, en) -> if st <= en then st else st - 1) . read2 . splitAt (div len 2) . show $ n
  where len = lenNum n

sumValidId :: (Int, Int) -> Int
sumValidId (a, b)
  | la == lb && mod (lenNum a) 2 /= 0 = 0
  | la == lb = sumEqualLength (getFirst a) (getLast b)
  | otherwise = sumValidId (a, (10 ^ la) - 1) + sumValidId ((10 ^ la), b)
  where
    la = lenNum a
    lb = lenNum b

part1 = readFile "input" >>= \file -> return . sum . map ((\x -> sumValidId (head x, last x)) . map read . ssplitAt '-') . ssplitAt ',' . strip $ file

divisors n = filter (\x -> n `mod` x == 0) $ [2..n]

isInvalid n = 0

sumInvalidId :: Int -> Int -> Int
sumInvalidId st en = 1
  where
    lst = lenNum st
    len = lenNum en
    dst = divisors lst
    den = divisors len
    
getFirst2 :: Int -> Int -> Int
getFirst2 n divisor = (\x -> (if look x >= n then x else x + 1 )). read . take (div len divisor) . show $ n
  where
    len = lenNum n
    look = read . concat . replicate divisor . show

getLast2 :: Int -> Int -> Int
getLast2 n divisor = (\x -> (if look x <= n then x else x - 1 )). read . take (div len divisor) . show $ n
  where
    len = lenNum n
    look :: Int -> Int
    look = read . concat . replicate divisor . show

look2 :: Int -> Int -> Int
look2 n divisor = read . concat . replicate divisor . show $ n

sumValidId2 :: Int -> Int -> Int
sumValidId2 a b
  | la == lb = sumEqualLength2 a b
  | otherwise = (sumValidId2 a (10 ^ la - 1)) + (sumValidId2 (10 ^ la) b)
  where
    la = lenNum a
    lb = lenNum b

listValidId :: Int -> Int -> [Int]
listValidId a b
  | la == lb = listEqualLength a b
  | otherwise = (listEqualLength a (10 ^ la - 1)) ++ (listEqualLength (10 ^ la) b)
  where
    la = lenNum a
    lb = lenNum b

sumEqualLength2 :: Int -> Int -> Int
sumEqualLength2 st en = sum . removeDuplicates . concat . map solve . divisors $ lst
  where
    lst = lenNum st
    -- cria todos os Invalidos para cada divisor diferente possivel.
    solve :: Int -> [Int]
    solve divsor = map (read . concat . replicate divsor . show) $ [fst..end]
      where
        fst = getFirst2 st divsor
        end = getLast2 en divsor

listEqualLength :: Int -> Int -> [Int]
listEqualLength st en = removeDuplicates . concat . map solve . divisors $ lst
  where
    lst = lenNum st
    -- cria todos os Invalidos para cada divisor diferente possivel.
    solve :: Int -> [Int]
    solve divsor = map (read . concat . replicate divsor . show) $ [fst..end]
      where
        fst = getFirst2 st divsor
        end = getLast2 en divsor

part2 = readFile "input" >>= \file -> return . sum . map (\x -> sumValidId2 (head x) (last x)) . map (map read . ssplitAt '-') . ssplitAt ',' . strip $ file
