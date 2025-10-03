level :: String -> Int
level = foldr action 0

action '(' = (+1)
action ')' = subtract 1
action _ = (+0)

removeNewlines :: String -> String
removeNewlines = filter (/= '\n')

level2 :: String -> Int
level2 = go 0 1
  where
    go :: Int -> Int -> String -> Int
    go _ _ [] = 0
    go level' position (c:cs)
      | action c level' == -1 = position
      | otherwise                = go (action c level') (position + 1) cs

main :: IO ()
main = do
  fileContent <- readFile "day01/input"
  (print . level) fileContent
  (print . level2) fileContent

