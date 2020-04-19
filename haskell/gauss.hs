import Debug.Trace

import Control.Parallel.Strategies

type Number = Double
type Vector = [Number]
type Row = [Number]
type Matrix = [Row]


backSubstitution :: Matrix -> Vector
backSubstitution [] = []
backSubstitution rows
    | last (last rows) == 0 = ((backSubstitution rows') ++ [0])
    | otherwise = ((backSubstitution rows') ++ [x])  where
    x = head (last rows) / last (last rows)
    rows' = map f (init rows)
        where f row = init (init row) ++ [last row - last (init row) * x]


nonZeroRowOnTop :: Matrix -> Matrix
nonZeroRowOnTop (row : rows)
    | (head row) /= 0 = (row : rows)
    | otherwise = nonZeroRowOnTop (rows ++ [row])


triangulate :: Matrix -> Matrix
triangulate [] = []
triangulate m = row : (triangulate rows)  where
    (row : rows') = nonZeroRowOnTop m
    rows = map f rows'
    f row'
        | (head row) == 0 = tail row'
        | otherwise = tail $ zipWith
            (-)
            (map (\e -> e * (head row) / (head row')) row')
            row


solver :: (Matrix, Vector) -> Vector
solver (a, b) = x  where
    ab = zipWith (\a b -> a ++ [b]) a b
    x = backSubstitution (triangulate ab)

main = do
    print
        (solver
            ( [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0], [1.0, 0.0, 1.0]]
            , [1.0, 1.0, 1.0]
            )
        )
