module Split where

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen pred = go []
  where
    go acc [] = [acc]
    go acc (c:cs)
      | pred c = acc : go [] cs
      | otherwise = go (acc ++ [c]) cs

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn x = splitWhen (==x)

chunksOf :: Int -> [a] -> [[a]]
chunksOf n s
  | length s < n = [s]
  | otherwise   = take n s : chunksOf n (drop n s)

splitOneOf :: (Eq a) => [a] -> [a] -> [[a]]
splitOneOf delims = splitWhen (`elem` delims)
