import Data.List.Split
import Debug.Trace

main = do
   content <- readFile "input.txt"
   let lines = splitOn "\n" content
   print(doTask lines)

doTask :: [String] -> String
doTask [] = ""
doTask (l:ls) =
   case isMatch l ls of
     Just x -> x
     Nothing -> doTask ls

isMatch :: String -> [String]-> Maybe String
isMatch _ [] = Nothing
isMatch n (h:hs) =
    case d2 n h of
        Just (a, b) ->  Just $ common a b
        Nothing -> isMatch n hs

d2 :: String -> String -> Maybe (String, String)
d2 a b
    | (1==) $ length $ c = Just (a, b)
    |otherwise = Nothing
    where
        c =  filter (\(x,y) -> x /= y) $ zip a b

common :: String -> String -> String
common a b = map (\(x,y) -> x) $ filter (\(x,y) -> x == y) $ zip a b
