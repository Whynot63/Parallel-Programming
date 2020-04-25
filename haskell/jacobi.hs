import Control.Parallel.Strategies


type Number = Double
type Vector = [Number]
type Row = [Number]
type Matrix = [Row]


norm :: Vector -> Number
norm [] = 0.0
norm v = sqrt $ sum [x*x | x <- v]


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
  | (norm $ zipWith (-) x_k x_k1) < eps = x_k 
  | otherwise = solver' (a, b, x_k1, eps, parCnt) where 
  x_k1 = iter (a, b, x_k, parCnt)


solver :: (Matrix, Vector, Number, Int) -> Vector
solver (a, b, eps, parCnt) = solver' (a, b, replicate (length b) 0, eps, parCnt)


main = do
  print
    (solver
      ([[10, 1, -1], [1, 10, -1], [-1, 1, 10]], [11, 10, 10], 0.1, 3)
    )
