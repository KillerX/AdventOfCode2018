import Data.List.Split
import Debug.Trace
import Text.Regex.Posix

import           Data.Set (Set)
import qualified Data.Set as S

main = do
   content <- readFile "input.txt"
   let ls = splitOn "\n" content
   let lines = take (length ls -1) ls
   print(doTask lines)

doTask :: [String] -> Claim
doTask ls =
    do
    let claims = map convert ls
    let claimSets = map makeSet claims
    let (ol, _) = findOverlapping claimSets S.empty S.empty
    findNonOverlapping claims ol

convert :: String -> Claim
convert s =
    do
    let (_, _, _, m) = s =~ "#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)" :: (String,String,String,[String])
    tuplify5 $ map (\x -> (read x :: Int)) m

tuplify5 :: [a] -> (a,a,a,a,a)
tuplify5 [a,b,c,d,e] = (a,b,c,d,e)

makeSet :: (Int, Int, Int, Int, Int) -> Set (Int, Int)
makeSet (_, x, y, w, h) = S.fromList [(xx,yy) | xx <- take w [x..], yy <- take h [y..]]

findOverlapping :: [Set(Int, Int)] -> Set(Int, Int) -> Set(Int, Int) -> (Set(Int, Int), Set(Int, Int))
findOverlapping (x:xs) ol nol =
    do
    let ol' = S.union ol $ S.intersection x nol
    let nol' = S.union nol x
    let (ol2, nol2) = findOverlapping xs ol' nol'
    (ol2, nol2)
findOverlapping [] a b = (a, b)

type Claim = (Int, Int, Int, Int, Int)

findNonOverlapping :: [Claim] -> Set(Int, Int) -> Claim
findNonOverlapping (x:xs) overlaps =
    do
    let isOverlap = S.size ( S.intersection overlaps $ makeSet x )
    case isOverlap == 0 of
        True -> x
        False -> findNonOverlapping xs overlaps
