import Data.List.Split

main = do
   content <- readFile "input.txt"
   let lines = splitOn "\n" content
   print (adjustFreqs lines)

convert :: String -> Int
convert ('-':digits) =
    -1 * (read digits :: Int)
convert (_:digits) =
    read digits :: Int
convert [] = 0

adjustFreqs :: [String] -> Int
adjustFreqs (x:xs) = ( convert x ) + adjustFreqs xs
adjustFreqs [] = 0
