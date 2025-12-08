import qualified Data.Graph as G
import Data.List (tails, sort)
euclid (a,b,c) (e,f,g) = (a-e)^2 + (b-f)^2 + (c-g)^2

main = do
  coords <- readFile "input" >>= pure . map (\x -> read $ "(" ++ x ++ ")") . lines
  let edges = take 1000 $ sort
    [ (euclid a c, a', c')
    | ((a', a), b) <- zip (zip [0..] coords) (tail $ tails coords)
    , (c', c) <- zip [a'+1..] b
    ]
  let g = G.buildG (0, 1000) [(b,c) | (a,b,c) <- edges]
  print $ product . take 3 . reverse . sort . map . length $ G.components g


