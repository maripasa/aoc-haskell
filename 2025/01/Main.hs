
flipDialCount r s
  | r > 0             = (c , s')
  | s == 0 && s' > 0  = (c - 1, s')
  | s > 0  && s' == 0 = (c + 1, s')
  | otherwise         = (c, s')
  where (c, s') = (\(a,b) -> (abs a, b)) $ ((s + r) `divMod` 100)

main = readFile "input" >>= \file -> return . foldl (\(c, h, s) r -> (\(c', s') -> (c + c', h + (fromEnum (s' == 0)), s')) $ flipDialCount r s) (0,0,50) . map (\(r:num) -> if r == 'R' then read num else -(read num)) . words $ file

