import Data.List

-- LONG LIST OF MAYBE USEFUL FUNCTIONS.
-- 
-- isCollinearPointsGrid (x1, y1) (x2, y2) (x3, y3) = (x1 == x2 && x2 == x3) || (y1 == y2 && y2 == y3)
-- corner a b c = not $ isCollinearPointsGrid a b c
-- corners xs = corners' ([last xs] ++ xs ++ [head xs])
--   where
--     corners' (a:b:c:rest)
--       | corner a b c = b : corners' (b:c:rest)
--       | otherwise    = corners' (b:c:rest)
--     corners' _ = []
-- inside xs ys = all (\(a, b) -> (isCollinearLinesGrid a b) || not (intersectsLinesGrid a b)) $ [(a,b) | a <- sides xs , b <- sides ys]
-- intersectsLinesGrid ((a, b), (c, d)) ((e, f), (g, h)) = (between e g a || between e g c) && (between b d f || between b d h)
-- isCollinearLinesGrid ((a,b),(c,d)) ((e,f), (g,h)) = (a == e && c == g) || (b == f && d == h)
--

-- ..............
-- .......#XWXW..
-- .......X...W..
-- ..#XXXX#...W..
-- ..X.....3OOOOO
-- ..#XXXXXX#.W..
-- .........X.W..
-- .........#XW..
-- ..............
--
-- point is not on top of side && line to right crosses

inside edges (a,b) = all (\lin -> (any (crosses lin) edges)) linesFromPoint
  where
    linesFromPoint =
      [ ((a, b), (a, b + 20000))
      , ((a, b), (a, b - 20000))
      , ((a, b), (a + 20000, b))
      , ((a, b), (a - 20000, b))
      ]

between a b x = x >= min a b && x <= max a b

crosses a b
  | vertical a && horizontal b = crossesHV b a
  | horizontal a && vertical b = crossesHV a b
  | otherwise = False
  where
    crossesHV ((a, b), (c, d)) ((e, f), (g, h)) = between a c e && between f h b

vertical ((a,b),(c,d)) = a == c
horizontal ((a,b), (c,d)) = b == d

-- i'll test the points to the right, so i want to count how many vertical lines it hits

--pointsRectDiag (a,b) (c,d) = [(a,b), (b,c), (c,d), (d,a)]

otherPoints (a,b) (c,d) = [(a,d), (c,b)]

comb xs = [(a,b) | (a, others) <- zip xs (tail $ tails xs), b <- others]

edges xs = side (xs ++ [head xs])
  where
    side (a:b:rest) = (a,b) : side (b:rest)
    side a = []

-- cartesian x, y, 0 based.

areaByDiag (a,b) (c,d) = (abs (a - c) + 1) * (abs (b - d) + 1)

main = do
  c <- readFile "input" >>= (\f -> return . map (\x -> read $ "(" ++ x ++ ")") . lines $ f)
  let coords :: [(Int, Int)]
      coords = c
  let diags = comb coords
      part1 = maximum . map (uncurry areaByDiag)

  let polygon = edges coords
      part2 = part1 . map (\[a,b] -> (a,b)) . filter (all (inside polygon)) . map (uncurry otherPoints) $ diags

  print part2


cellChar :: [((Int,Int),(Int,Int))] -> (Int,Int) -> Char
cellChar polygon p =
    if inside polygon p
    then 'X'
    else '.'

makeGrid :: [((Int,Int),(Int,Int))] -> [[Char]]
makeGrid polygon =
    [ [ cellChar polygon (x,y) | x <- [0..13] ]
    | y <- [0..10]
    ]

printGrid :: [[Char]] -> IO ()
printGrid = mapM_ putStrLn

drawPolygon :: [((Int,Int),(Int,Int))] -> IO ()
drawPolygon poly = printGrid (makeGrid poly)
