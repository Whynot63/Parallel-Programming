import Control.Parallel.Strategies
import Debug.Trace


type Number = Double
type Vector = [Number]
type Row = [Number]
type Matrix = [Row]


norm :: Vector -> Number
norm [] = 0.0
norm v = sqrt $ sum [x*x | x <- v]


iter :: (Matrix, Vector, Vector, Integer) -> Vector
iter (a, b, x_k, parCnt)
    | parCnt == 1 = zipWith3 (\a_i b_i i -> 
      1 / (a_i !! i) * (
        b_i + (a_i !! i) * x_k !! i - sum(zipWith (*) a_i x_k ))
    ) a b [0 .. length a]
    | otherwise = [0.0]


solver' :: (Matrix, Vector, Vector, Number) -> Vector
solver' (a, b, x_k, eps)
  | (norm $ zipWith (-) x_k x_k1) < eps = x_k 
  | otherwise = solver' (a, b, x_k1, eps) where 
  x_k1 = iter (a, b, x_k, 1)


solver :: (Matrix, Vector, Number) -> Vector
solver (a, b, eps) = solver' (a, b, replicate (length b) 0, eps)


main = do
  print
    (solver
      ([[10, 1, -1], [1, 10, -1], [-1, 1, 10]], [11, 10, 10], 0.1)
    )
