import Data.List.Split

main = do
   content <- readFile "input.txt"
   let lines = splitOn "\n" content
   print(doTask lines)

doTask :: [String] -> Int
doTask ls = do
    let a = map twos ls
    let b = map threes ls
    (*) (sum a) (sum b)

xses :: Int -> [Char] -> Int
xses p (x:xs)
    | (length (filter (==x) xs))== p = 1
    | otherwise = xses p $ filter (/=x) xs
xses _ [] = 0

twos :: [Char] -> Int
twos = xses 1

threes :: [Char] -> Int
threes = xses 2
