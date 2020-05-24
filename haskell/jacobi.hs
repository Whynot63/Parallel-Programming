import Data.Time
import System.Environment
import Control.Parallel.Strategies
import Text.Printf
import Data.Ord
import Data.List

type Number = Double
type Vector = [Number]
type Row = [Number]
type Matrix = [Row]


norm :: Vector -> Number
norm [] = 0.0
norm v = sqrt $ sum [x*x | x <- v]

breakCondition :: (Vector, Vector, Number) -> Bool
breakCondition (v1, v2, eps) = all (<eps) $ zipWith (\a b -> abs(a - b) / abs(a)) v1 v2


partialIter :: (Matrix, Vector, Vector, Int, Int) -> Vector
partialIter (a, b, x_k, from, to) = x_k1_from_to where
  a_from_to = drop from $ take to a
  b_from_to = drop from $ take to b

  x_k1_from_to = zipWith3 (\a_i b_i i -> 
      1 / (a_i !! i) * (
        b_i + (a_i !! i) * x_k !! i - sum(zipWith (*) a_i x_k ))
    ) a_from_to b_from_to [from .. to]


iter :: (Matrix, Vector, Vector, Int) -> Vector
iter (a, b, x_k, parCnt)
    | parCnt == 1 = partialIter (a, b, x_k, 0, length a)
    | otherwise = concat ([partialIter (a, b, x_k, from i, to i) | i <- [0..parCnt-1]] `using` parList rdeepseq) where 
      n = length b
      from = \i -> i * n `div` parCnt
      to = \i -> (i + 1) * n `div` parCnt


solver' :: (Matrix, Vector, Vector, Number, Int) -> Vector
solver' (a, b, x_k, eps, parCnt)
  | breakCondition (x_k, x_k1, eps) = x_k 
  | otherwise = solver' (a, b, x_k1, eps, parCnt) where 
  x_k1 = iter (a, b, x_k, parCnt)


solver :: (Matrix, Vector, Number, Int) -> Vector
solver (a, b, eps, parCnt) = solver' (a, b, replicate (length b) 0, eps, parCnt)


-- Stolen from https://stackoverflow.com/a/14550568
absArgmax :: Vector -> Int
absArgmax xs = snd $ maximumBy (comparing $ abs . fst) (zip xs [0..]);


moveNthElemToHead :: (Vector, Int) -> Vector
moveNthElemToHead ([], _) = []
moveNthElemToHead (v, n) = (nthElem: before ++ after) where 
  nthElem = v !! n
  before = take n v
  after = tail $ drop n v


prepareMatrix' :: Matrix -> Matrix
prepareMatrix' (row: rows) 
  | rows == [] = [row]
  | otherwise = (preparedRow: prepareMatrix' preparedRows) where 
    maxIdx = absArgmax row
    preparedRow = moveNthElemToHead (row, maxIdx)
    preparedRows = map (\r -> moveNthElemToHead (r, maxIdx)) rows

prepareMatrix :: Matrix -> (Matrix, Vector)
prepareMatrix m = (prepM, idxs) where 
  n = length m
  mWithIdxs = m ++ [[0..]]
  prepMWithIdxs = prepareMatrix' mWithIdxs
  prepM = init prepMWithIdxs  
  idxs = take n $ last prepMWithIdxs

readNumber :: String -> Number
readNumber = read

readInt :: String -> Int
readInt = read


main :: IO ()
main = do
  start <- getCurrentTime
  (parCnt : nEquations : nUnknowns : aFile : bFile : _) <- getArgs

  let n = read nEquations :: Int
  let m = read nUnknowns :: Int

  aText <- readFile aFile
  let aFlat = map readNumber . words $ aText

  let a = [[aFlat !! (i * m + j) | j <- [0..m-1]] | i <- [0..n-1]]

  bText <- readFile bFile
  let b = map readNumber . words $ bText

  readEnd <- getCurrentTime
  printf "Read time: %.3f seconds\n" (realToFrac (diffUTCTime readEnd start):: Double)

  let (aPrepared, idx) = prepareMatrix a
  prepareEnd <- getCurrentTime
  printf "Preparation time: %.3f seconds\n" (realToFrac (diffUTCTime prepareEnd readEnd):: Double)

  let x = solver(a, b, 0.0001, (read parCnt :: Int))

  solverEnd <- getCurrentTime
  printf "Solver time: %.3f seconds\n" (realToFrac (diffUTCTime solverEnd prepareEnd):: Double)

  let xFinall = map snd $ sort $ zip idx x
  sortEnd <- getCurrentTime
  printf "Sort time: %.3f seconds\n" (realToFrac (diffUTCTime sortEnd solverEnd):: Double)

  writeFile "solution.txt" $ show $ xFinall
  writeEnd <- getCurrentTime
  printf "Write time: %.3f seconds\n" (realToFrac (diffUTCTime writeEnd sortEnd):: Double)

  printf "Total time: %.3f seconds\n" (realToFrac (diffUTCTime writeEnd start):: Double)



