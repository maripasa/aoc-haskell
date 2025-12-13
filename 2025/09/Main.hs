import Data.List
import Debug.Trace
import Control.DeepSeq
import Data.Ord

type Pt  = (Double, Double)
type Seg = (Pt, Pt)

readCoords :: String -> [Pt]
readCoords s = map (\x -> read ("(" ++ x ++ ")")) (lines s)

edges :: [Pt] -> [Seg]
edges xs = zip xs (tail (cycle xs))

comb :: [a] -> [(a,a)]
comb xs = [(a,b) | (a,rest) <- zip xs (tail (tails xs)), b <- rest]

isLineRect :: Seg -> Bool
isLineRect diag = vertical diag || horizontal diag

smallerRect ((x1, y1), (x2, y2)) = ((min x1 x2 + 0.5, min y1 y2 + 0.5), (max x1 x2 - 0.5, max y1 y2 - 0.5))
biggerRect ((x1, y1), (x2, y2)) = ((min x1 x2 - 0.5, min y1 y2 - 0.5), (max x1 x2 + 0.5, max y1 y2 + 0.5))

otherPoints ((x1,y1),(x2,y2)) = [(x1, y1), (x1, y2), (x2, y2), (x2, y1)]

areaByDiag :: Seg -> Double
areaByDiag ((x1, y1), (x2, y2)) =
  (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)

vertical ((x1, y1), (x2, y2)) = x1 == x2
horizontal ((x1, y1), (x2, y2)) = y1 == y2

inside poly square = isValid poly-- 0
  where
    ((x1,y1),(x2,y2)) = head square
    a = (x1 + x2) / 2
    b = (y1 + y2) / 2
    rightLine = ((a, b), (a + 20000, b))
    isValid [] = True
    isValid (e:es) = not (any (intersects e) square) && isValid (es)-- (n + fromEnum (intersects e rightLine))

intersects lin1 lin2
  | vertical lin1 == vertical lin2 = False
  | horizontal lin1 = crossesHV lin1 lin2
  | otherwise       = crossesHV lin2 lin1

crossesHV ((hx1,hy),(hx2, _)) ((vx,vy1),(_,vy2)) = (between hx1 hx2 vx) && (between vy1 vy2 hy)

between a b x = min a b <= x && x <= max a b 
  
main = do
  file <- readFile "input"
  let coords = readCoords file
  let poly   = edges coords
      diags  = comb $ coords
      x = round . areaByDiag . head . map (biggerRect)
        . filter (inside poly . edges . otherPoints)
        . map (smallerRect) . reverse . sortBy (comparing areaByDiag) . filter (not . isLineRect) $ diags
  print x
  return x
