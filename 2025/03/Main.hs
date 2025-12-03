-- PLEASE MOVE THIS TO LIB
ssplitAt del str = splitAt "" str
  where
  splitAt acc (c:cs)
    | c == del  = reverse acc : splitAt "" cs
    | otherwise = splitAt (c:acc) cs
  splitAt acc [] = [reverse acc]

-- must be divisor, actually
splitIn n [] = []
splitIn n lst = take n lst : splitIn n (drop n lst)
-- chunks!!!

strip = reverse . remWhite . reverse . remWhite
  where remWhite = dropWhile (\x -> elem x " \n")

rsplitAt n lst = splitAt (length lst - n) lst

chooseBat :: Int -> [Int] -> [Int]
chooseBat n bat = reverse (go [] (n - 1) bat)
  where
    go :: [Int] -> Int -> [Int] -> [Int]
    go acc 0 bat = maximum bat : acc
    go acc n bat
      = (\(rfst, lst, mst) -> go (mst : acc) (n - 1) (rfst ++ lst))
        . (\(fst, lst, mst) -> ((tail . dropWhile (/= mst) $ fst), lst, mst))
          . (\(fst, lst) -> (fst, lst, maximum fst))--, max fst))
            . splitAt (length bat - n) $ bat

read2 :: [String] -> [Int]
read2 = map read

solve :: Int -> IO Int                                                                                        -- READ DIGITS FUNCTION
solve n = readFile "input" >>= \file -> return . sum . map (\lstb -> read . concat . map show . chooseBat n . read2 . splitIn 1 $ lstb) . ssplitAt '\n' . strip $ file
