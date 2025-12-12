import Data.List (tails)

type Pt  = (Int, Int)
type Seg = (Pt, Pt)

readCoords :: String -> [Pt]
readCoords s = map (\x -> read ("(" ++ x ++ ")")) (lines s)

edges :: [Pt] -> [Seg]
edges xs = zip xs (tail (cycle xs))

comb :: [a] -> [(a,a)]
comb xs = [(a,b) | (a,rest) <- zip xs (tail (tails xs)), b <- rest]

areaByDiag :: Pt -> Pt -> Int
areaByDiag (a,b) (c,d) =
  (abs (a - c) + 1) * (abs (b - d) + 1)

pointsRect :: Pt -> Pt -> [Pt]
pointsRect (a,b) (c,d) =
  [ (x,y)
  | x <- [min a c .. max a c]
  , y <- [min b d .. max b d]
  ]

pointOnSegment :: Pt -> Seg -> Bool
pointOnSegment (px,py) ((x1,y1),(x2,y2))
  | x1 == x2 =
      px == x1 && py >= min y1 y2 && py <= max y1 y2
  | y1 == y2 =
      py == y1 && px >= min x1 x2 && px <= max x1 x2
  | otherwise = False

rayIntersects :: Pt -> Seg -> Bool
rayIntersects (px,py) ((x1,y1),(x2,y2))
  | y1 == y2 = False
  | otherwise =
      let x = x1
          ymin = min y1 y2
          ymax = max y1 y2
      in  py > ymin
       && py <= ymax
       && x > px
       
pointInPolygon :: [Seg] -> Pt -> Bool
pointInPolygon segs p =
  if any (pointOnSegment p) segs
    then True
    else odd (length [() | seg <- segs, rayIntersects p seg])

main :: IO ()
main = do
  file <- readFile "input"
  let coords = readCoords file

  let poly   = edges coords
      diags  = comb coords

  let part1Max =
        if null diags
          then 0
          else maximum (map (uncurry areaByDiag) diags)

  let validDiags =
        [ (p,q)
        | (p,q) <- diags
        , all (pointInPolygon poly) (pointsRect p q)
        ]

  let part2 =
        if null validDiags
          then 0
          else maximum (map (uncurry areaByDiag) validDiags)

  print part2
